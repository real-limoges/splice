;;;; harness.lisp: synthetic order-processing workload with injected faults.
;;;; The flagship patch set moves order math from float dollars to rational
;;;; minor units (three functions, a method, and a class redefinition).
;;;;
;;;; Fault kinds:
;;;;   :latent          fails only for VIP-coded orders
;;;;   :load-correlated fails above a canary traffic share
;;;;   :slow-burn       latency regression, no errors
;;;;   :delayed-onset   healthy for N calls, then fails
;;;;   :migration       forward migration fails on corrupted orders
;;;;   nil              clean patch (false-positive baseline)

(in-package #:splice/harness)

;;; ---------------------------------------------------------------------
;;; Deterministic 64-bit LCG, so every session is reproducible from its seed.

(defstruct (rng (:constructor %make-rng)) (state 88172645463325252))

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

;; pristine references for RESET-WORKLOAD
(defvar *pristine-fns*
  (list 'price-order #'price-order 'apply-discount #'apply-discount
        'total-invoice #'total-invoice 'process-order #'process-order))
(defvar *pristine-order-shape* nil)

;;; ---------------------------------------------------------------------
;;; Fault injection

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

;;; ---------------------------------------------------------------------
;;; Flagship patch set: float dollars -> rational minor units. New code
;;; writes through to FLOAT-TOTAL so old code stays correct during canary.

(defvar *fault-rng* nil "RNG serving MAYBE-INJECT draws, seeded per session.")

(defun write-through-float (order cents)
  "Keep FLOAT-TOTAL in sync while it exists (the canary union shape)."
  (when (slot-exists-p order 'float-total)
    (setf (slot-value order 'float-total) (/ cents 100.0))))

(defun new-price-order (order)
  (maybe-inject :price order (if *fault-rng* (rng-uniform *fault-rng*) 0.5))
  (let ((cents (round (* 100 (order-item-count order)
                         (rational (order-unit-price order))))))
    (setf (slot-value order 'amount-minor-units) cents
          (order-status order) :priced)
    (write-through-float order cents)
    cents))

(defun new-apply-discount (order)
  (maybe-inject :discount order 0.5)
  (let ((cents (round (* (slot-value order 'amount-minor-units)
                         (discount-multiplier (order-discount-code order))))))
    (setf (slot-value order 'amount-minor-units) cents
          (order-status order) :discounted)
    (write-through-float order cents)
    cents))

(defun new-total-invoice (order)
  (maybe-inject :invoice order 0.5)
  (let ((cents (slot-value order 'amount-minor-units)))
    (setf (order-status order) :invoiced)
    (write-through-float order cents)
    cents))

(defun forward-migration (instance old-values)
  (when (and (fault-active-p :migration)
             (let ((ft (getf old-values 'float-total)))
               (and ft (minusp ft))))
    (error "migration fault: cannot derive minor units from corrupted total ~a"
           (getf old-values 'float-total)))
  (setf (slot-value instance 'amount-minor-units)
        (round (* 100 (rational (or (getf old-values 'float-total) 0))))))

(defun backward-migration (instance new-values)
  (setf (slot-value instance 'float-total)
        (/ (or (getf new-values 'amount-minor-units) 0) 100.0)))

(defun make-test-order (&key (cents 1099) (code nil))
  (make-instance 'order :id -1 :item-count 1 :unit-price (/ cents 100.0)
                        :discount-code code))

(defun register-currency-patch-set (&key (ramp '(5 25 100)) (alpha 0.01d0)
                                      (beta 0.05d0) (min-n 50) (max-n 2000)
                                      paired-fallback)
  "Register a fresh flagship patch set. Faults come from *FAULT*, so one set
serves every session."
  (unregister-patch-set 'currency-migration)
  (register-patch-set
   (splice::make-patch-set
    :name 'currency-migration
    :description "orders: float dollars -> rational minor units"
    :ticket "SPLICE-42"
    :ramp ramp :alpha alpha :beta beta
    :sprt-min-n min-n :sprt-max-n max-n
    :paired-fallback paired-fallback
    :fn-patches
    (list (splice::make-fn-patch
           :name 'price-order :new-fn #'new-price-order
           :new-source '(function new-price-order)
           :new-source-hash (splice:sha256-hex "new-price-order-v2"))
          (splice::make-fn-patch
           :name 'apply-discount :new-fn #'new-apply-discount
           :new-source '(function new-apply-discount)
           :new-source-hash (splice:sha256-hex "new-apply-discount-v2"))
          (splice::make-fn-patch
           :name 'total-invoice :new-fn #'new-total-invoice
           :new-source '(function new-total-invoice)
           :new-source-hash (splice:sha256-hex "new-total-invoice-v2")))
    :method-patches
    (list (splice::make-method-patch
           :gf-name 'print-total :specializer-names '(order stream)
           :new-fn (lambda (order stream)
                     (multiple-value-bind (dollars cents)
                         (floor (slot-value order 'amount-minor-units) 100)
                       (format stream "$~d.~2,'0d" dollars cents)))
           :new-source '(print-total-minor-units)
           :new-source-hash (splice:sha256-hex "print-total-v2")))
    :class-patches
    (list (splice::make-class-patch
           :class-name 'order
           :add-slots '((amount-minor-units :initform 0))
           :drop-slots '(float-total)
           :migrate-forward #'forward-migration
           :migrate-backward #'backward-migration))
    :contracts
    (list (splice::make-contract
           :name 'invoice-is-exact
           :check-fn (lambda ()
                       (let ((o (make-test-order :cents 1099))
                             (*fault* nil)) ; contracts test the code, not the fault
                         (funcall (splice:new 'price-order) o)
                         (= 1099 (funcall (splice:new 'total-invoice) o))))
           :source '(invoice-is-exact))
          (splice::make-contract
           :name 'pipeline-conserves-discount
           :check-fn (lambda ()
                       (let ((o (make-test-order :cents 1000 :code "TEN"))
                             (*fault* nil))
                         (funcall (splice:new 'price-order) o)
                         (funcall (splice:new 'apply-discount) o)
                         (= 900 (funcall (splice:new 'total-invoice) o))))
           :source '(pipeline-conserves-discount))
          (splice::make-contract
           :name 'old-new-differential
           :check-fn (lambda ()
                       (let ((o1 (make-test-order :cents 2499 :code "VIP"))
                             (o2 (make-test-order :cents 2499 :code "VIP"))
                             (*fault* nil))
                         (price-order o1) (apply-discount o1) (total-invoice o1)
                         (funcall (splice:new 'price-order) o2)
                         (funcall (splice:new 'apply-discount) o2)
                         (<= (abs (- (round (* 100 (order-float-total o1)))
                                     (funcall (splice:new 'total-invoice) o2)))
                             1)))
           :source '(old-new-differential))))))

;;; ---------------------------------------------------------------------
;;; Session driver

(defstruct session-result
  final-state transitions tasks-run mixed-executions veto-p
  detection-observations reversal-report pre-existing-snapshot
  post-rollback-comparison wallclock-ms)

(defun reset-workload ()
  "Restore the pristine pre-patch world so every session starts identical."
  (loop for (name fn) on *pristine-fns* by #'cddr
        do (setf (fdefinition name) fn))
  ;; restore the original class shape captured at load time
  (destructuring-bind (slots supers) *pristine-order-shape*
    (splice::apply-class-shape 'order slots supers))
  ;; restore the original print-total method if a promoted shim replaced it
  (splice::eval-method-redefinition
   '(defmethod print-total ((order order) (stream stream))
     (format stream "$~,2f" (order-float-total order))))
  (unregister-patch-set 'currency-migration)
  (clrhash splice::*migration-registry*)
  (clrhash splice::*tracked-classes*)
  (sb-thread:with-mutex (splice::*migration-lock*)
    (clrhash splice::*migration-journal*)
    (clrhash splice::*canary-population*))
  (values))

(defun snapshot-order (order)
  (list :id (order-id order)
        :float-total (order-float-total order)
        :status (order-status order)
        :discount-code (order-discount-code order)))

(defun make-pre-existing-orders (rng count &key corrupted)
  "Orders that predate the patch, for migration to move and rollback to
restore. CORRUPTED orders fail forward migration under :migration."
  (let ((orders (loop for i from 1 to count
                      collect (let ((o (make-instance 'order
                                                      :id i
                                                      :item-count (rng-range rng 1 9)
                                                      :unit-price (/ (rng-range rng 100 9999) 100.0)
                                                      :discount-code (case (mod (rng-next rng) 5)
                                                                       (0 "TEN") (1 "VIP")
                                                                       (t nil)))))
                                (price-order o)
                                o))))
    (dotimes (i (or corrupted 0))
      (let ((o (nth (* 3 i) orders)))
        (setf (order-float-total o) -5.0 (order-status o) :corrupted)))
    orders))

(defun run-canary-session (&key (fault nil) (seed 42) (max-tasks 20000)
                             (ramp '(5 25 100)) (alpha 0.01d0) (beta 0.05d0)
                             (min-n 50) (max-n 2000)
                             (pre-existing 40) (threads 1)
                             (touch-pre-existing-every 11)
                             paired-fallback quiet)
  "Run one patch lifecycle under synthetic load; return a SESSION-RESULT.
Expects an open audit log."
  (reset-workload)
  (reset-coherence-tracking)
  (let* ((rng (make-rng seed))
         (*fault-rng* (make-rng (+ seed 7)))
         (*fault* (when fault (make-fault :kind fault)))
         (orders (make-pre-existing-orders
                  rng pre-existing
                  :corrupted (when (eq fault :migration) 3)))
         (pre-snapshot (mapcar #'snapshot-order orders))
         (start (get-internal-real-time))
         (audit-start-seq (if splice:*audit-log*
                              (splice::audit-log-seq splice:*audit-log*)
                              0))
         (ps (register-currency-patch-set
              :ramp ramp :alpha alpha :beta beta :min-n min-n :max-n max-n
              :paired-fallback paired-fallback))
         (tasks-run (list 0))
         (stop (list nil)))
    (handler-case (install ps)
      (patch-vetoed (e)
        (return-from run-canary-session
          (make-session-result :final-state :vetoed :veto-p t
                               :transitions (princ-to-string e)))))
    (flet ((run-one (i task-rng)
             ;; think time keeps load tracking the ramp when canary is slow
             (when (> threads 1) (sleep 0.0002))
             (when (fault-active-p :load-correlated)
               (fault-window-note *fault* :total))
             (with-task (:id i :swallow-errors t)
               (let ((o (if (zerop (mod i touch-pre-existing-every))
                            ;; touch old orders to drive forward migration
                            (nth (mod (rng-next task-rng) (length orders)) orders)
                            (make-instance 'order
                                           :id (+ 1000 i)
                                           :item-count (rng-range task-rng 1 9)
                                           :unit-price (/ (rng-range task-rng 100 9999) 100.0)
                                           :discount-code (case (mod (rng-next task-rng) 5)
                                                            (0 "TEN") (1 "VIP") (t nil))))))
                 (process-order o))))
           (terminal-p ()
             ;; once decided, stop load so completion isn't mutex-starved
             (member (patch-set-state ps)
                     '(:promoted :rolled-back :frozen :vetoed
                       :promoting :rolling-back))))
      (if (> threads 1)
          ;; threads don't inherit dynamic bindings: rebind the shared
          ;; fault and give each worker its own PRNG
          (let* ((fault-obj *fault*)
                 (workers
                   (loop for w from 0 below threads
                         collect (let ((wrng (make-rng (+ seed 100 w))))
                                   (sb-thread:make-thread
                                    (lambda ()
                                      (let ((*fault* fault-obj)
                                            (*fault-rng* wrng))
                                        (loop for i = (sb-ext:atomic-incf (car tasks-run))
                                              until (or (car stop) (> i max-tasks))
                                              do (run-one i wrng)
                                                 (when (terminal-p)
                                                   (setf (car stop) t)))))
                                    :name (format nil "harness-worker-~a" w))))))
            (mapc #'sb-thread:join-thread workers))
          (loop for i from 1 to max-tasks
                until (terminal-p)
                do (run-one i rng) (setf (car tasks-run) i))))
    (wait-for-state ps '(:promoted :rolled-back :frozen) :timeout 30)
    (let ((result
            (make-session-result
             :final-state (patch-set-state ps)
             :transitions (patch-set-lifecycle-history ps)
             :tasks-run (car tasks-run)
             :mixed-executions (splice::mixed-execution-count)
             :detection-observations (detection-observations-from-audit audit-start-seq)
             :reversal-report (reverse-migration-report 'currency-migration)
             :pre-existing-snapshot pre-snapshot
             :post-rollback-comparison
             (when (eq (patch-set-state ps) :rolled-back)
               (compare-rollback-exactness orders pre-snapshot ps))
             :wallclock-ms (round (* 1000 (- (get-internal-real-time) start))
                                  internal-time-units-per-second))))
      (unless quiet
        (format t "~&  [~a seed=~a] ~a after ~a tasks (~ams)~@[ detection-n=~a~]~%"
                (or fault :clean) seed (session-result-final-state result)
                (session-result-tasks-run result)
                (session-result-wallclock-ms result)
                (session-result-detection-observations result)))
      result)))

(defun detection-observations-from-audit (&optional (after-seq 0))
  "SPRT sample count at the rollback decision, read from audit log entries
after AFTER-SEQ."
  (when splice:*audit-log*
    (let ((*package* (find-package :splice)))
      (loop for line in (splice::audit-log-lines splice:*audit-log*)
            for entry = (ignore-errors (read-from-string line))
            when (and entry (<= (getf entry :seq 0) after-seq))
              do (return nil)   ; lines are newest-first; past the session
            when (and entry (eq (getf entry :event) :rollback-decision))
              do (let ((trigger (getf entry :trigger)))
                   (when (and (consp trigger) (eq (first trigger) :sprt-reject))
                     (return (getf (second trigger) :n))))))))

(defun compare-rollback-exactness (orders pre-snapshot ps)
  "Check rollback exactness: fdefinitions EQ the originals, the class shape
is original, and pre-existing orders match their pre-patch snapshot."
  (let ((fn-ok (every (lambda (fp)
                        (eq (fdefinition (splice::fn-patch-name fp))
                            (splice::fn-patch-old-fn fp)))
                      (splice::patch-set-fn-patches ps)))
        (shape-ok (null (set-exclusive-or
                         '(id item-count unit-price discount-code
                           float-total status)
                         (mapcar #'sb-mop:slot-definition-name
                                 (sb-mop:class-direct-slots
                                  (find-class 'order))))))
        (restored (loop for o in orders
                        for snap in pre-snapshot
                        count (and (equal (order-float-total o)
                                          (getf snap :float-total))
                                   (equal (order-status o)
                                          (getf snap :status))))))
    (list :fdefinitions-eq fn-ok
          :class-shape-original shape-ok
          :snapshot-restored restored
          :snapshot-total (length orders))))

(defun coherence-violations () (splice::mixed-execution-count))
(defun reset-coherence-tracking ()
  (setf (car splice::*mixed-execution-cell*) 0))

(defun run-workload (&key (tasks 500) (seed 1))
  "Plain workload with no patch in flight (baseline / overhead measurement)."
  (let ((rng (make-rng seed)))
    (loop for i from 1 to tasks
          do (with-task (:id i :swallow-errors t)
               (process-order (make-instance 'order
                                             :id i :item-count (rng-range rng 1 9)
                                             :unit-price (/ (rng-range rng 100 9999) 100.0)
                                             :discount-code (case (mod (rng-next rng) 5)
                                                              (0 "TEN") (1 "VIP") (t nil))))))))

;; capture the pristine class shape at load time, after ORDER is defined
(setf *pristine-order-shape*
      (multiple-value-list (splice::capture-class-shape 'order)))
