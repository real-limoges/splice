(in-package #:splice-orders/demo)

(defparameter *lane-prefixes*
  (append orders:*thread-prefixes*
          '("demo-session" "splice-promote-" "splice-rollback-")))

(defun layout-pending-p (instance)
  "True while INSTANCE is waiting for a class update. Reads the layout not slots."
  (not (null (sb-kernel:layout-invalid (sb-kernel:layout-of instance)))))

(defun read-set (ps)
  (list :name (patch-set-name ps)
        :state (patch-set-state ps)
        :fraction (patch-set-current-fraction ps)
        :stage-index (patch-set-stage-index ps)
        :ramp (patch-set-ramp ps)
        :pinned-old (patch-set-pinned-old ps)
        :pinned-new (patch-set-pinned-new ps)))

(defun installed-world (fp)
  "Which implementation FP's name currently resolves to"
  (let ((current (fdefinition (fn-patch-name fp))))
    (cond ((and (fn-patch-shim fp) (eq current (fn-patch-shim fp))) :split)
          ((eq current (fn-patch-new-fn fp)) :new)
          ((eq current (fn-patch-old-fn fp)) :old)
          (t :foreign))))

(defun class-shape (class-name)
  "Which of the demo's three types CLASS-NAME has, just from the metadata"
  (let ((names (mapcar #'sb-mop:slot-definition-name
                       (sb-mop:class-slots (find-class class-name)))))
    (cond ((and (member 'orders::float-total names)
                (member 'orders::amount-minor-units names))
           :union)
          ((member 'orders::amount-minor-units names) :final)
          (t :original))))

(defun read-migration-tables (orders)
  "Journal membership per order + canary-born counts per route - in one fell swoop"
  (sb-thread:with-mutex (*migration-lock*)
    (list :journaled (mapcar (lambda (o) (nth-value 1 (gethash o *migration-journal*))) orders)
          :canary-born (count-by-route *canary-population*))))

(defun order-state (order journaled-p)
  (cond ((layout-pending-p order) :pending)
        (journaled-p :migrated)
        (t :current)))

(defun thread-pins (thread)
  "THREAD's current pins as (set-name . route) or () between tasks"
  (let ((epochs (sb-thread:symbol-value-in-thread 'splice::*task-epochs* thread nil)))
    (mapcar (lambda (pin) (cons (patch-set-name (first pin)) (second pin)))
            epochs)))

(defun new-audit-entries (log after-seq)
  "Entries newer than AFTER-SEQ, oldest first. Also whether the tail was trimmed past it."
  (let ((lines (sb-thread:with-mutex ((splice::audit-log-lock log))
                 (copy-list (splice::audit-log-lines log)))))
    (collect-newer (mapcar #'read-audit-line lines) after-seq)))

(defun read-image (session after-seq)
  "Readings for one frame, :between-sessions, or :unreadable"
  (cond ((null session) :between-sessions)
        (t (handler-case (read-session session after-seq)
             (error () :unreadable)))))

(defun sample-once (snapshot sinks)
  "Fold one frame into a SNAPSHOT and hand the result to every sink"
  (let ((readings (read-image *live-session* (getf snapshot :audit-seq 0))))
    (if (keywordp readings)
        snapshot
        (let* ((started (getf (getf readings :session) :started))
               (next (next-snapshot (snapshot-for-session snapshot started) readings)))
          (dolist (sink sinks next)
            (funcall sink next))))))

(defun run-sampler (sinks &key (period 0.1) (running (constantly t)))
  (loop with snapshot = (empty-snapshot)
        while (funcall running)
        do (setf snapshot (sample-once snapshot sinks))
           (sleep period)))
