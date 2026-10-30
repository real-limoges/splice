;;;; types.lisp: patch-set structs, the registry, and lifecycle-state
;;;; predicates.

(in-package #:splice)

;;; ---------------------------------------------------------------------
;;; Structs and registry

(defstruct fn-patch
  name            ; symbol naming the function
  new-fn          ; compiled new implementation
  new-source      ; source form of the new implementation (for the audit log)
  new-source-hash
  old-fn          ; captured original fdefinition (in case of rollback)
  old-source-hash ; hash of the original's identity (printed repr)
  shim)           ; installed dispatch shim, while in canary

(defstruct method-patch
  gf-name qualifiers specializer-names
  new-fn new-source new-source-hash
  old-method      ; captured method object (add-method'd back on rollback)
  shim-method)    ; installed routing method, while in canary

(defstruct class-patch
  class-name
  add-slots       ; defclass-style slot specs to add
  drop-slots      ; slot names dropped by the new shape (retained during canary)
  migrate-forward ; (instance old-values-plist) -> populates new slots
  migrate-backward; (instance new-values-plist) -> synthesizes old slots
  lossy-p         ; author declares reversal lossy => rollback needs a human
  old-slot-specs  ; captured canonicalized direct-slot specs of the original
  old-superclass-names)

(defstruct contract
  name check-fn source)

(defstruct (patch-set (:print-object print-patch-set))
  name description ticket
  fn-patches method-patches class-patches contracts
  (ramp '(5 25 100))
  (state :registered)
  (current-fraction 0)
  (stage-index -1)            ; index into RAMP; -1 = not yet canarying
  (lock (sb-thread:make-mutex))
  (paired-fallback nil)       ; opt-in
  (alpha 0.01d0) (beta 0.05d0)
  (sprt-min-n 50) (sprt-max-n 2000)
  sprt-error sprt-latency     ; per-stage tests, reset on stage entry
  control-ring
  (confound-threshold 0.15)   ; control failure rate above this => FROZEN
  (runtime-contract-interval 200) ; run contracts every N canary observations
  (pinned-new 0) (pinned-old 0)   ; in-flight task counts, per route
  (hard-trigger nil)          ; set by anomaly capture; forces rollback
  (history '())               ; (state . timestamp) transitions, newest first
  (freeze-reason nil)
  (last-reversal-report nil)  ; reversal counts from the last rollback
  (sealed nil))               ; journal released; rollback no longer offered

(defvar *patch-sets* (make-hash-table :test #'eq #+sbcl :synchronized #+sbcl t)
  "Registry of every patch set this image has seen, terminal states included.")

(defvar *deferred-effects*)             ; unbound outside WITH-SET-LOCK, so stray DEFER errors

(defun print-patch-set (ps stream)
  (print-unreadable-object (ps stream :type t)
    (format stream "~a ~a ~a%" (patch-set-name ps) (patch-set-state ps)
            (patch-set-current-fraction ps))))

(defun find-patch-set (name)
  (gethash name *patch-sets*))

;;; ---------------------------------------------------------------------
;;; Lifecycle state

(defun stage-keyword (fraction)
  (intern (format nil "CANARY-~d" fraction) :keyword))

(defun arm-hard-trigger (ps trigger)
  "Arm PS's hard trigger unless one is already armed; the first cause wins."
  (sb-ext:compare-and-swap (patch-set-hard-trigger ps) nil trigger))

(defun take-hard-trigger (ps)
  "PS's armed trigger or NIL, leaving the slot clear."
  (loop for seen = (patch-set-hard-trigger ps)
        when (or (null seen)
                 (eq   seen (sb-ext:compare-and-swap (patch-set-hard-trigger ps)
                                                     seen nil)))
          return seen))

(defun canary-stage-p (state)
  (let ((s (symbol-name state)))
    (and (> (length s) 7) (string= "CANARY-" (subseq s 0 7)))))

;; Rollback and promotion each take two states: the first until the drain
;; ends (a freeze can still stop it), the second once the completion thread
;; has claimed the set and started changing code.
(defun rolling-back-p (state)
  (member state '(:rolling-back :restoring)))

(defun promoting-p (state)
  (member state '(:promoting :swapping)))

(defun active-canary-sets ()
  "Patch sets that fresh tasks must pin against: canary stages, plus
promoting ones (pin :new across the swap) and rolling-back or :frozen ones
(pin :old)."
  (let ((out '()))
    (maphash (lambda (name ps)
               (declare (ignore name))
               (when (or (canary-stage-p (patch-set-state ps))
                         (rolling-back-p (patch-set-state ps))
                         (promoting-p (patch-set-state ps))
                         (eq :frozen (patch-set-state ps)))
                 (push ps out)))
             *patch-sets*)
    out))

;;; ---------------------------------------------------------------------
;;; Set lock

(defun defer (thunk)
  "Run THUNK once the enclosing WITH-SET-LOCK releases"
  (push thunk *deferred-effects*))

(defmacro with-set-lock ((ps) &body body)
  "BODY under PS's lock, then deferred effects (in order) after release."
  `(let ((*deferred-effects* '()))
     (multiple-value-prog1
         (sb-thread:with-mutex ((patch-set-lock ,ps)) ,@body)
       (mapc #'funcall (reverse *deferred-effects*)))))
