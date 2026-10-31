;;;; sampler.lisp: reads the image without disturbing it and folds each frame into a snapshot.

(in-package #:splice-orders/demo)

(defparameter *lane-prefixes*
  (append orders:*thread-prefixes*
          '("demo-session" "splice-promote-" "splice-rollback-"))
  "Thread-name prefixes that get a lane. Every other thread is left out")

(defvar *demo-running* nil "True when sampler thread should keep sampling")
(defvar *sampler-thread* nil "Sample thread last started or NIL")

;;; ---------------------------------------------------------------------
;;; Image readings

(defun layout-pending-p (instance)
  "True while INSTANCE is waiting for a class update. Reads the layout not slots."
  (not (null (sb-kernel:layout-invalid (sb-kernel:layout-of instance)))))

(defun count-by-route (population)
  "Canary born counts per creating route. Caller holds *MIGRATION-LOCK*"
  (loop for birth being the hash-values of population
        count (eq (cdr birth) :new) into new
        count (not (eq (cdr birth) :new)) into old
        finally (return (list :old old :new new))))

(defun read-migration-tables (orders)
  "Journal membership per order + canary-born counts in one lock"
  (sb-thread:with-mutex (splice::*migration-lock*)
    (list :journaled (mapcar (lambda (o) (nth-value 1 (gethash o splice::*migration-journal*)))
                             orders)
          :canary-born (count-by-route splice::*canary-population*))))

(defun classify-order (pending-p journaled-p)
  "The dot an order gets from its two readings"
  (cond (pending-p :pending)
        (journaled-p :migrated)
        (t :current)))

(defun read-orders (orders)
  "The :ORDERS vector and :CANARY-BORN counts. Read before the tables, so a touch
in between shows as one frame of :PENDING never as a wrong :CURRENT."
  (let* ((pending (mapcar #'layout-pending-p orders))
         (tables (read-migration-tables orders)))
    (list :orders (coerce (mapcar #'classify-order pending (getf tables :journaled))
                          'vector)
          :canary-born (getf tables :canary-born))))

(defun read-set (ps)
  (list :name (splice:patch-set-name ps)
        :state (splice:patch-set-state ps)
        :fraction (splice::patch-set-current-fraction ps)
        :stage-index (splice::patch-set-stage-index ps)
        :ramp (splice::patch-set-ramp ps)
        :pinned-old (splice::patch-set-pinned-old ps)
        :pinned-new (splice::patch-set-pinned-new ps)))

(defun installed-world (fp)
  "Which implementation FP's name currently resolves to"
  (let ((current (fdefinition (splice::fn-patch-name fp))))
    (cond ((and (splice::fn-patch-shim fp) (eq current (splice::fn-patch-shim fp))) :split)
          ((eq current (splice::fn-patch-new-fn fp)) :new)
          ((eq current (splice::fn-patch-old-fn fp)) :old)
          (t :foreign))))

(defun read-function (fp)
  (list :name (splice::fn-patch-name fp)
        :installed (installed-world fp)))

(defun class-shape (class-name)
  "Which of the demo's three types CLASS-NAME has, just from the metadata"
  (let ((names (mapcar #'sb-mop:slot-definition-name
                       (sb-mop:class-slots (find-class class-name)))))
    (cond ((and (member 'orders::float-total names)
                (member 'orders::amount-minor-units names))
           :union)
          ((member 'orders::amount-minor-units names) :final)
          (t :original))))

(defun class-phase (class-name)
  "Migration phase registered for CLASS-NAME or :NONE"
  (let ((entry (gethash class-name splice::*migration-registry*)))
    (if entry (cdr entry) :none)))

(defun read-class (class-name)
  (list :name class-name
        :shape (class-shape class-name)
        :phase (class-phase class-name)))

(defun read-sprt-bounds (ps)
  "Reject and accept bounds and the min-n floor. Both streams share them."
  (let ((sprt (splice::patch-set-sprt-error ps)))
    (list :upper (float (splice:sprt-upper sprt) 1.0)
          :lower (float (splice:sprt-lower sprt) 1.0)
          :min-n (splice::sprt-min-n sprt))))

;;; ---------------------------------------------------------------------
;;; Threads

(defun name-starts-with-p (prefix name)
  (and (<= (length prefix) (length name))
       (string= prefix name :end2 (length prefix))))

(defun lane-thread-p (thread)
  (let ((name (sb-thread:thread-name thread)))
    (and name (some (lambda (prefix) (name-starts-with-p prefix name))
                    *lane-prefixes*))))

(defun thread-pins (thread)
  "THREAD's current pins as (set-name . route) or () between tasks"
  (let ((epochs (sb-thread:symbol-value-in-thread 'splice::*task-epochs* thread nil)))
    (mapcar (lambda (pin) (cons (splice:patch-set-name (first pin)) (second pin)))
            epochs)))

(defun read-lane (thread)
  (list :name (sb-thread:thread-name thread)
        :pins (thread-pins thread)))

(defun read-lanes ()
  "One lane per session thread, sorted by name so they stay in their
place."
  (sort (mapcar #'read-lane (remove-if-not #'lane-thread-p (sb-thread:list-all-threads)))
        #'string< :key (lambda (lane) (getf lane :name))))

;;; ---------------------------------------------------------------------
;;; Audit tail

(defun read-audit-line (line)
  "One audit line as a plist, read the same way as MAP-AUDIT-ENTRIES reads the file"
  (with-standard-io-syntax
    (let ((*package* (find-package '#:splice))
          (*read-eval* nil))
      (read-from-string line))))

(defun entry-seq (entry) (getf entry :seq))

(defun copy-audit-tail (log)
  "LOG's in memory lines. Newest first, copied under its own lock because AUDIT trims them"
  (sb-thread:with-mutex ((splice::audit-log-lock log))
    (copy-list (splice::audit-log-lines log))))

(defun tail-status (newer lines cursor)
  ":TRIMMED when every line is newer than CURSOR and the oldest is not the next
one after it (entries were lost) otherwise :COMPLETE"
  (if (and newer
           (= (length newer) (length lines))
           (> (entry-seq (first (last newer))) (1+ cursor)))
      :trimmed
      :complete))

(defun collect-newer (lines cursor)
  "Entries in LINES (newest first!) above CURSOR (oldest first! Not confusing!)
Reads lines only until the first one at or below the cursor.
Then it's the second value from TAIL-STATUS"
  (let ((newer (loop for line in lines
                     for entry = (read-audit-line line)
                     while (> (entry-seq entry) cursor)
                     collect entry)))
    (values (reverse newer) (tail-status newer lines cursor))))

(defun new-audit-entries (cursor)
  "Audit entries above CURSOR (oldest first) and :COMPLETE, :TRIMMED, :NO-LOG"
  (let ((log splice:*audit-log*))
    (if log
        (collect-newer (copy-audit-tail log) cursor)
        (values '() :no-log))))

(defun newest-seq (entries cursor)
  (reduce #'max entries :key #'entry-seq :initial-value cursor))

(defun sprt-steps (entries set-name)
  (remove-if-not (lambda (entry)
                   (and (eq (getf entry :event) :sprt-step)
                        (eq (getf entry :patch-set) set-name)))
                 entries))

;;; ---------------------------------------------------------------------
;;; One frame

(defun elapsed-ms (started)
  (round (* 1000 (- (get-internal-real-time) started))
         internal-time-units-per-second))

(defun read-session-info (session)
  (list :act *current-act*
        :seed (orders:live-session-seed session)
        :fault (orders:live-session-fault session)
        :threads (orders:live-session-threads session)
        :started (orders:live-session-started session)
        :audit-start-seq (orders:live-session-audit-start-seq session)))

(defun read-session (session cursor faults)
  "One frame's readings from the image, the plist NEXT-SNAPSHOT folds it."
  (let ((ps (orders:live-session-patch-set session)))
    (multiple-value-bind (entries tail) (new-audit-entries cursor)
      (append (list :elapsed-ms (elapsed-ms (orders:live-session-started session))
                    :audit-seq (newest-seq entries cursor)
                    :paused-at (orders:live-session-paused-at session)
                    :session (read-session-info session)
                    :set (read-set ps)
                    :functions (mapcar #'read-function (splice::patch-set-fn-patches ps))
                    :class (read-class 'orders:order)
                    :threads (read-lanes)
                    :bounds (read-sprt-bounds ps)
                    :sprt-steps (sprt-steps entries (splice:patch-set-name ps))
                    :tail tail
                    :faults faults)
              (read-orders (orders:live-session-orders session))))))

(defun frame-cursor (previous session)
  "The audit seq to read above: the last one folded or the session's start"
  (max (getf previous :audit-seq) (orders:live-session-audit-start-seq session)))

(defun fault-string (what condition)
  "WHAT and the condition's type Never the condition's report which could print an order
and migrate it"
  (format nil "~a: ~a" what (type-of condition)))

(defun add-fault (fault faults)
  "FAULTS with FAULT in front, keeping the newest *five*"
  (let ((all (cons fault faults)))
    (subseq all 0 (min 5 (length all)))))

(defun fold-frame (previous session faults)
  "Snapshot after PREVIOUS or a fault string when the image could not be read.
Things like a registry race or a class mid-redefinition"
  (handler-case
      (next-snapshot previous (read-session session (frame-cursor previous session) faults))
    (error (e) (fault-string "frame skipped" e))))

(defun call-sink (sink snapshot)
  "Hand SNAPSHOT to SINK. Returns :OK or a fault string when SINK signals"
  (handler-case (progn (funcall sink snapshot) :ok)
    (error (e) (fault-string "sink failed" e))))

(defun call-sinks (sinks snapshot)
  "Hand SNAPSHOT to every sink in order. Returns the faults, newest *five*"
  (let ((faults (remove :ok (mapcar (lambda (sink) (call-sink sink snapshot)) sinks))))
    (subseq faults 0 (min 5 (length faults)))))

;;; ---------------------------------------------------------------------
;;; Sampler thread

(defun sample-session (snapshot faults session sinks)
  "SAMPLE-ONCE for a published SESSION. A frame read while the session was withdrawn is dropped
since the next session may already be resetting the workload"
  (let* ((previous (snapshot-for-session snapshot (orders:live-session-started session)))
         (next (fold-frame previous session faults)))
    (cond ((stringp next) (values snapshot (add-fault next faults)))
          ((eq session orders:*live-session*) (values next (call-sinks sinks next)))
          (t (values snapshot faults)))))

(defun sample-once (snapshot faults sinks)
  "Returns SNAPSHOT to fold from next time and the faults the next snapshot reports.
Between session it calls no sink"
  (let ((session orders:*live-session*))
    (if session
        (sample-session snapshot faults session sinks)
        (values snapshot faults))))

(defun run-sampler (sinks &key (period 0.1) (running (constantly t)))
  "Sample every PERIOD seconds while RUNNING is true. Returns :STOPPED"
  (loop with snapshot = (empty-snapshot)
        with faults = '()
        while (funcall running)
        do (multiple-value-setq (snapshot faults) (sample-once snapshot faults sinks))
           (sleep period)
        finally (return :stopped)))

(defun thread-alive-p (thread)
  (and thread (sb-thread:thread-alive-p thread)))

(defun start-sampler (sinks &key (period 0.1))
  "Start the sampler thread feeding SINKS. Returns the thread or :ALREADY-RUNNING
when one runs, since two samplers would mean two writers"
  (if (thread-alive-p *sampler-thread*)
      :already-running
      (progn (setf *demo-running* t)
             (setf *sampler-thread*
                   (sb-thread:make-thread
                    (lambda () (run-sampler sinks :period period
                                                  :running (lambda () *demo-running*)))
                    :name "demo-sampler")))))

(defun stop-sampler ()
  "Stop the sampler and wait for its frame to progress to finish.
Returns :STOPPED, :NOT-JOINED (didn't finish in 5 seconds) or :NOT-RUNNING"
  (if (thread-alive-p *sampler-thread*)
      (progn (setf *demo-running* nil)
             (sb-thread:join-thread *sampler-thread* :default :not-joined :timeout 5))
      :not-running))
