;;;; app.lisp: the application under patch, a synthetic order-processing
;;;; service, with its injected faults. The faults are called from inside the
;;;; patch's new implementations (flagship.lisp), so they live with the app.
;;;;
;;;; Fault kinds:
;;;;   :latent          fails only for VIP-coded orders
;;;;   :load-correlated fails above a canary traffic share
;;;;   :slow-burn       latency regression, no errors
;;;;   :delayed-onset   healthy for N calls, then fails
;;;;   :migration       forward migration fails on corrupted orders
;;;;   nil              clean patch (false-positive baseline)

(in-package #:splice-orders)

;;; ---------------------------------------------------------------------
;;; Globals and the structs they hold

(defstruct (rng (:constructor %make-rng)) (state 88172645463325252))

(defstruct fault
  kind
  (latent-code "VIP")           ; :latent trips on this discount code
  (share-threshold 0.15)        ; :load-correlated trips above this share
  (window-min-total 100)        ; ... once the window has enough traffic
  (onset-calls 250)             ; :delayed-onset healthy for this many calls
  (onset-error-rate 0.30)
  (slow-ms 3.0)                 ; :slow-burn extra latency
  (call-count (list 0))         ; shared counter (cons cell for atomicity)
  (window (list 0 0 0)))        ; (start-tick total-calls canary-calls)

(defvar *fault* nil "The active injected fault, or NIL for a clean patch.")

(defvar *fault-rng* nil "RNG serving MAYBE-INJECT draws, seeded per session.")

;; pristine references for RESET-WORKLOAD. *PRISTINE-FNS* is bound further
;; down, once the original implementations it captures exist.
(defvar *pristine-fns*)
(defvar *pristine-order-shape* nil)

;;; ---------------------------------------------------------------------
;;; Deterministic 64-bit LCG, so every session is reproducible from its seed.

(defun make-rng (seed) (%make-rng :state (logand (+ seed 2862933555777941757)
                                                 #xffffffffffffffff)))

(defun rng-next (rng)
  (setf (rng-state rng)
        (logand (+ (* (rng-state rng) 6364136223846793005) 1442695040888963407)
                #xffffffffffffffff))
  (ldb (byte 32 16) (rng-state rng)))

(defun rng-uniform (rng)
  (/ (rng-next rng) (float (expt 2 32) 1d0)))

(defun rng-range (rng lo hi)
  (+ lo (mod (rng-next rng) (1+ (- hi lo)))))

;;; --- The application under patch: an order-processing service

(defclass order ()
  ((id :initarg :id :initform 0 :accessor order-id)
   (item-count :initarg :item-count :initform 1 :accessor order-item-count)
   (unit-price :initarg :unit-price :initform 1.0 :accessor order-unit-price)
   (discount-code :initarg :discount-code :initform nil
                  :accessor order-discount-code)
   (float-total :initform 0.0 :accessor order-float-total)
   (status :initform :new :accessor order-status)))

(defun order-cents (order)
  "Read the patched representation (only exists under the union/new slots)."
  (slot-value order 'amount-minor-units))

(defun discount-multiplier (code)
  (cond ((equal code "TEN") 9/10)
        ((equal code "VIP") 17/20)
        (t 1)))

;; --- original implementations (float dollars; classic representation debt)

(defun price-order (order)
  (setf (order-float-total order)
        (* (order-item-count order) (order-unit-price order))
        (order-status order) :priced)
  (order-float-total order))

(defun apply-discount (order)
  (setf (order-float-total order)
        (* (order-float-total order)
           (float (discount-multiplier (order-discount-code order))))
        (order-status order) :discounted)
  (order-float-total order))

(defun total-invoice (order)
  (setf (order-float-total order)
        (/ (fround (* 100 (order-float-total order))) 100)
        (order-status order) :invoiced)
  (order-float-total order))

(defgeneric print-total (order stream))
(defmethod print-total ((order order) (stream stream))
  (format stream "$~,2f" (order-float-total order)))

(defun process-order (order)
  "The task body; mixing old and new implementations here corrupts totals.
Agnostic to the money representation, so it survives the patch without
joining the set."
  (price-order order)
  (apply-discount order)
  (let ((total (total-invoice order)))
    (with-output-to-string (s) (print-total order s))
    total))

;; bind the *PRISTINE-FNS* declared at the top, now that the originals exist.
;; DEFVAR binds only on first load, so a reload never captures patched code.
(defvar *pristine-fns*
  (list 'price-order #'price-order 'apply-discount #'apply-discount
        'total-invoice #'total-invoice 'process-order #'process-order))

;; capture the pristine class shape at load time, after ORDER is defined
(setf *pristine-order-shape*
      (multiple-value-list (splice::capture-class-shape 'order)))

;;; ---------------------------------------------------------------------
;;; Fault injection

(defun all-fault-kinds () '(:latent :load-correlated :slow-burn
                            :delayed-onset :migration))

(defun fault-description (kind)
  (ecase kind
    ((nil) "clean patch, no injected fault")
    (:latent "fails only for VIP-coded orders (input-dependent subclass)")
    (:load-correlated
     "fails only once canary code exceeds ~15% of traffic (pool exhaustion)")
    (:slow-burn "no errors; +3ms latency on every canary invoice")
    (:delayed-onset "healthy for 250 calls, then 30% error rate")
    (:migration "forward instance migration fails on corrupted orders")))

(defun fault-active-p (kind) (and *fault* (eq (fault-kind *fault*) kind)))

(defun fault-window-note (f which)
  "Count total vs canary calls in a ~10ms bucket. Rotation races are
tolerable; the window is statistical."
  (let* ((now (get-internal-real-time))
         (bucket-ticks (max 1 (round (* 0.010 internal-time-units-per-second))))
         (w (fault-window f)))
    (when (> (- now (first w)) bucket-ticks)
      (setf (fault-window f) (setf w (list now 0 0))))
    (when (eq which :total) (incf (second w)))
    (when (eq which :canary) (incf (third w)))))

(defun maybe-inject (site order rng-draw)
  "Called from inside the NEW implementations at the named SITE."
  (when *fault*
    (let ((f *fault*))
      (case (fault-kind f)
        (:latent
         (when (and (eq site :discount)
                    (equal (order-discount-code order) (fault-latent-code f)))
           (error "latent fault: discount table corrupt for code ~a"
                  (order-discount-code order))))
        (:load-correlated
         (when (eq site :price)
           ;; saturates above a canary share: invisible at 5%, trips at 25%
           (fault-window-note f :canary)
           (destructuring-bind (start total canary) (fault-window f)
             (declare (ignore start))
             (when (and (>= total (fault-window-min-total f))
                        (> (/ canary (max 1 total))
                           (fault-share-threshold f)))
               (error "load-correlated fault: pool exhausted at ~,1f% canary share"
                      (* 100.0 (/ canary (max 1 total))))))))
        (:slow-burn
         (when (eq site :invoice)
           (sleep (/ (fault-slow-ms f) 1000))))
        (:delayed-onset
         (when (eq site :price)
           (let ((n (sb-ext:atomic-incf (car (fault-call-count f)))))
             (when (and (> n (fault-onset-calls f))
                        (< rng-draw (fault-onset-error-rate f)))
               (error "delayed-onset fault: resource exhausted after ~a calls"
                      n)))))))))
