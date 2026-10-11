;;;; router.lisp: per-task epoch pinning
;;;; A task pins a route (:old or :new) per active patch set at entry and
;;;; keeps it for its lifetime. A logical task can never observe a
;;;; mixture of old and new from one patch set.

(in-package #:splice)

(defvar *task-id* nil "Identity of the current task, NIL outside task scope.")
(defvar *task-epochs* nil
  "List of (patch-set route . observed-route) pins made at task entry.
OBSERVED-ROUTE is written by fixed-arity shims on every call: one assoc
serves both the route lookup and the coherence trace.")
(defvar *task-shim-routes* nil
  "Alist (patch-set . route) actually observed by shims during this task:
compared against the pin at exit to *verify* coherence, not just assert it.")
(defvar *task-anomaly-noted* nil
  "Alist (patch-set . t): first anomaly per set per task already recorded,
so one error unwinding through nested shims counts once.")
(defvar *task-counter* 0)
(defvar *task-counter-lock* (sb-thread:make-mutex))
(defvar *task-sample-every* 50
  "Log every Nth task's pinned routes to the audit log.")

(defvar *mixed-execution-cell* (list 0)
  "Global count of coherence violations ever observed (cons cell so
ATOMIC-INCF applies). The design invariant is that this stays 0.")
(defun mixed-execution-count () (car *mixed-execution-cell*))
(defun note-mixed-execution () (sb-ext:atomic-incf (car *mixed-execution-cell*)))

(defun next-task-id ()
  (sb-thread:with-mutex (*task-counter-lock*)
    (incf *task-counter*)))

(defun routing-draw (task-id set-name)
  "Deterministic per-(task, set) draw in [0,100). SHA-256-based, not SXHASH,
so the draw is reproducible across images and offline from the audit log."
  (mod (parse-integer
        (subseq (sha256-hex (format nil "~a|~a" task-id set-name)) 0 8)
        :radix 16)
       100))

(defun pin-route (ps task-id)
  "Route decision for a fresh task against PS, honoring its lifecycle state.
Canarying sets draw against the ramp fraction; a promoting set pins :new
(the swap to new is imminent and irreversible); draining/frozen pin :old."
  (case (patch-set-state ps)
    (:promoting :new)
    (t (if (and (canary-stage-p (patch-set-state ps))
                (< (routing-draw task-id (patch-set-name ps))
                   (patch-set-current-fraction ps)))
           :new
           :old))))

(defun task-route-for (ps)
  "The current task's pinned route for PS; :old outside task scope
(background threads never canary)."
  (let ((pin (assoc ps *task-epochs* :test #'eq)))
    (if pin (cadr pin) :old)))

(defun current-route (set-name)
  (let ((ps (find-patch-set set-name)))
    (and ps (task-route-for ps))))

(defun note-shim-route (ps route)
  "Coherence verification: record the route a shim actually took. A task
that ever observes two different routes for one set is a mixed execution."
  (let ((prior (assoc ps *task-shim-routes* :test #'eq)))
    (cond ((null prior)
           (push (cons ps route) *task-shim-routes*))
          ((not (eq (cdr prior) route))
           (note-mixed-execution)
           (sb-thread:with-mutex ((patch-set-lock ps))
             (incf (patch-set-mixed-executions ps)))
           (audit :coherence-violation
                  :patch-set (patch-set-name ps) :task *task-id*
                  :pinned (cdr prior) :observed route)))))

(defun note-anomaly (ps condition)
  "Record a canary anomaly (error out of :new-routed code), once per task
per set. Storage conditions and migration failures arm the hard trigger."
  (unless (assoc ps *task-anomaly-noted* :test #'eq)
    (push (cons ps t) *task-anomaly-noted*)
    (signal 'canary-anomaly :patch-set ps :task *task-id* :cause condition)
    (audit :canary-anomaly
           :patch-set (patch-set-name ps) :task *task-id*
           :cause (princ-to-string condition)
           :cause-type (string (type-of condition))))
  (when (or (typep condition 'storage-condition)
            (typep condition 'migration-failure))
    (setf (patch-set-hard-trigger ps)
          (list (if (typep condition 'storage-condition)
                    :storage-condition
                    :migration-failure)
                (princ-to-string condition)))))

(defun call-with-task (id thunk &key (swallow-errors nil))
  "Run THUNK as one logical task: pin a route per active patch set, track
in-flight counts for quiescence, verify coherence at exit, and report the
task's outcome (success/failure + latency) to every pinned set's evaluator.
Establishes the FALLBACK-TO-OLD restart (retries the whole task all-old)."
  (let* ((*task-id* (or id (next-task-id)))
         (sets (active-canary-sets))
         (*task-epochs* (mapcar (lambda (ps) (list ps (pin-route ps *task-id*)))
                                sets))
         (*task-shim-routes* '())
         (*task-anomaly-noted* '())
         (start (get-internal-real-time))
         (outcome :success)
         (condition nil))
    (dolist (pin *task-epochs*)
      (sb-thread:with-mutex ((patch-set-lock (car pin)))
        (if (eq (cadr pin) :new)
            (incf (patch-set-pinned-new (car pin)))
            (incf (patch-set-pinned-old (car pin))))))
    ;; sampled task records: enough to re-derive routing decisions offline
    ;; (the draw is deterministic in (task-id, set-name))
    (when (and *task-epochs* (integerp *task-id*)
               (zerop (mod *task-id* *task-sample-every*)))
      (audit :task-sample :task *task-id*
                          :pins (mapcar (lambda (pin)
                                          (list :patch-set (patch-set-name (car pin))
                                                :route (cadr pin)
                                                :fraction (patch-set-current-fraction
                                                           (car pin))
                                                :state (patch-set-state (car pin))))
                                        *task-epochs*)))
    (unwind-protect
         (restart-case
             ;; the recording must happen in whichever handler actually
             ;; receives the error: an inner HANDLER-CASE would unwind
             ;; before an outer HANDLER-BIND's recorder ever ran
             (if swallow-errors
                 (handler-case (funcall thunk)
                   (error (e) (setf outcome :failure condition e) nil))
                 (handler-bind ((error (lambda (e)
                                         (setf outcome :failure condition e))))
                   (funcall thunk)))
           (fallback-to-old ()
             :report "Retry this task with every patch set routed to old code."
             ;; fresh pins AND a fresh trace: the deliberate all-old retry
             ;; must not read as a coherence violation of the original pins
             (let ((*task-epochs* (mapcar (lambda (pin) (list (car pin) :old))
                                          *task-epochs*))
                   (*task-shim-routes* '()))
               (funcall thunk))))
      ;; coherence check: every route a shim actually took must match the
      ;; pin, from the method/variadic shim trace and the in-pin marks
      (dolist (obs *task-shim-routes*)
        (let ((pin (assoc (car obs) *task-epochs* :test #'eq)))
          (unless (and pin (eq (cdr obs) (cadr pin)))
            (note-mixed-execution))))
      (dolist (pin *task-epochs*)
        (when (and (cddr pin) (not (eq (cddr pin) (cadr pin))))
          (note-mixed-execution)))
      ;; release in-flight pins before reporting: a reported failure may
      ;; initiate rollback, whose completion thread waits on these counts
      (dolist (pin *task-epochs*)
        (sb-thread:with-mutex ((patch-set-lock (car pin)))
          (if (eq (cadr pin) :new)
              (decf (patch-set-pinned-new (car pin)))
              (decf (patch-set-pinned-old (car pin))))))
      (let ((latency-ms (/ (* 1000.0 (- (get-internal-real-time) start))
                           internal-time-units-per-second)))
        (dolist (pin *task-epochs*)
          (report-outcome (car pin) (cadr pin) (eq outcome :failure) latency-ms
                          :condition condition))))))

(defmacro with-task ((&key id (swallow-errors nil)) &body body)
  `(call-with-task ,id (lambda () ,@body) :swallow-errors ,swallow-errors))
