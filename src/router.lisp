;;;; router.lisp: per-task epoch pinning
;;;; A task pins :old or :new per active patch set at entry and keeps it, so
;;;; it never sees a mixture of old and new from one set.

(in-package #:splice)

(defvar *task-id* nil "Identity of the current task, NIL outside task scope.")
(defvar *task-epochs* nil
  "List of (patch-set route) pins made at task entry.")
(defvar *task-anomaly-noted* nil
  "Alist (patch-set . t) of sets whose anomaly this task already recorded,
so an error unwinding through nested shims counts once.")
(defvar *task-counter-cell* (list 0)
  "Task ids handed out so far. A cons for ATOMIC-INCF.")
(defvar *task-sample-every* 50
  "Log every Nth task's pinned routes to the audit log.")

(defun next-task-id ()
  (1+ (sb-ext:atomic-incf (car *task-counter-cell*))))

(defun routing-draw (task-id set-name)
  "Deterministic draw in [0,100) per (task, set). SHA-256 rather than SXHASH
so it reproduces across images and from the audit log."
  (mod (parse-integer
        (subseq (sha256-hex (format nil "~a|~a" task-id set-name)) 0 8)
        :radix 16)
       100))

(defun pin-route (ps task-id)
  "Route for a fresh task. Canarying sets draw against the ramp fraction,
a promoting set pins :new, and anything else pins :old."
  (case (patch-set-state ps)
    ((:promoting :swapping) :new)
    (t (if (and (canary-stage-p (patch-set-state ps))
                (< (routing-draw task-id (patch-set-name ps))
                   (patch-set-current-fraction ps)))
           :new
           :old))))

(defun pin-and-count (ps task-id)
  "Pin PS for a fresh task and count it in flight, all as one step."
  (with-set-lock (ps)
    (let ((route (pin-route ps task-id)))
      (if (eq route :new)
          (incf (patch-set-pinned-new ps))
          (incf (patch-set-pinned-old ps)))
      (list ps route))))

(declaim (inline task-route-for))
(defun task-route-for (ps)
  "The current task's pinned route for PS, or :old outside a task.
Asked for Inline since every shim call asks it"
  (let ((pin (assoc ps *task-epochs* :test #'eq)))
    (if pin (cadr pin) :old)))

(defun current-route (set-name)
  (let ((ps (find-patch-set set-name)))
    (and ps (task-route-for ps))))

(defun note-anomaly (ps condition)
  "Record an error from :new-routed code, once per task per set. Storage
conditions and migration failures arm the hard trigger."
  (unless (assoc ps *task-anomaly-noted* :test #'eq)
    (push (cons ps t) *task-anomaly-noted*)
    (signal 'canary-anomaly :patch-set ps :task *task-id* :cause condition)
    (audit :canary-anomaly
           :patch-set (patch-set-name ps) :task *task-id*
           :cause (princ-to-string condition)
           :cause-type (string (type-of condition))))
  (when (or (typep condition 'storage-condition)
            (typep condition 'migration-failure))
    (arm-hard-trigger ps (list (if (typep condition 'storage-condition)
                                   :storage-condition
                                   :migration-failure)
                               (princ-to-string condition)))))

(defun call-with-task (id thunk &key (swallow-errors nil))
  "Run THUNK as one task: pin routes, count in-flight tasks, and report the
outcome to each pinned set. The FALLBACK-TO-OLD restart retries the whole task
on old code."
  (let* ((*task-id* (or id (next-task-id)))
         (sets (active-canary-sets))
         (*task-epochs* (mapcar (lambda (ps) (pin-and-count ps *task-id*)) sets))
         (*task-anomaly-noted* '())
         (start (get-internal-real-time))
         (outcome :success)
         (condition nil))
    ;; sampled pins, enough to re-derive routing offline
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
             ;; record in whichever handler receives the error: a HANDLER-CASE
             ;; would unwind before an outer HANDLER-BIND could record it
             (if swallow-errors
                 (handler-case (funcall thunk)
                   (error (e) (setf outcome :failure condition e) nil))
                 (handler-bind ((error (lambda (e)
                                         (setf outcome :failure condition e))))
                   (funcall thunk)))
           (fallback-to-old ()
             :report "Retry this task with every patch set routed to old code."
             (let ((*task-epochs* (mapcar (lambda (pin) (list (car pin) :old))
                                          *task-epochs*)))
               (funcall thunk))))
      ;; release pins before reporting: a rollback would wait on these counts
      (dolist (pin *task-epochs*)
        (with-set-lock ((car pin))
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
