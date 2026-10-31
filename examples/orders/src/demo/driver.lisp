;;;; driver.lisp

(in-package #:splice-orders/demo)

(defvar *gate* (make-gate)
  "The presenter gate the current act's pace function + held task
wait on")

(defvar *demo-log-path* nil
  "Audit log opened at pre-flight - act 3 verifies it")

(defvar *session-thread* nil
  "Demo-session thread of the current or last act")

(defvar *recorder* nil
  "The open recording as (:path :sink :close) or NIL between recordings")

(defun act-settings (act)
  "Everything that differs between acts. Act 1's drain gives time to explain
the held task"
  (ecase act
    (1 '(:seed 1001 :fault nil :threads 3 :pre-existing 200 :drain 120))
    (2 '(:seed 2002 :fault :latent :threads 3 :pre-existing 200 :drain 10))))

(defun act-pace (act)
  (ecase act
    (1 #'act-1-pace)
    (2 #'stage-pace)))

(defun act-1-pace (stage)
  "Pause at each stage. For stage 1, start the held :old task before
pausing"
  (when (= stage 1)
    (orders:start-held-task
     (orders:live-session-patch-set orders:*live-session*)
     #'await-release))
  (await-gate *gate* :next-stage))

(defun stage-pace (stage)
  "Pause at each stage until the presenter opens :next-stage"
  (declare (ignore stage))
  (await-gate *gate* :next-stage))

(defun await-release ()
  "The held task's hold: wait for Release on whichever gate is current"
  (await-gate *gate* :release-held))

(defun run-act (act)
  "Run ACT's canary session in this thread. Drain timeout is set first."
  (let ((settings (act-settings act)))
    (setf splice::*drain-timeout-seconds* (getf settings :drain))
    (orders:run-canary-session
     :seed (getf settings :seed)
     :fault (getf settings :fault)
     :threads (getf settings :threads)
     :pre-existing (getf settings :pre-existing)
     :quiet t
     :pace (act-pace act)
     :settle-timeout (+ (getf settings :drain) 30))))

(defun session-alive-p ()
  "True as long as either the last act's session thread or held task is alive"
  (and (or (and *session-thread* (sb-thread:thread-alive-p *session-thread*))
           (find "orders-straggler" (sb-thread:list-all-threads)
                 :key #'sb-thread:thread-name :test #'equal))
       t))

(defun start-recording (act)
  "Open a fresh recording for ACT and keep it in *recorder*. Returns its path"
  (let* ((settings (act-settings act))
         (path (recording-path act (getf settings :seed))))
    (multiple-value-bind (sink close)
        (make-recorder path (recording-header act settings))
      (setf *recorder* (list :path path :sink sink :close close))
      path)))

(defun stop-recording ()
  "Stop the sampler and close the (open) recording. Return its path.
:none when nothing was recording or :sampler-stuck when the sampler did not
join and may still write"
  (if (eq :not-joined (stop-sampler))
      :sampler-stuck
      (close-recording)))

(defun close-recording ()
  (if *recorder*
      (let ((path (getf *recorder* :path)))
        (funcall (getf *recorder* :close))
        (setf *recorder* nil)
        path)
      :none))

(defun act-sinks (sinks)
  "SINKS, with the open recording's sink in front (if there is one)"
  (if *recorder*
      (cons (getf *recorder* :sink) sinks)
      sinks))

(defun reset-between-acts ()
  "Fresh gate and default drain timeout. Returns :reset or :session-running"
  (if (session-alive-p)
      :session-running
      (progn (setf *gate* (make-gate)
                   splice::*drain-timeout-seconds* 10)
             :reset)))

(defun start-act (act sinks)
  "Start ACT with SINKS. Returns recording's path, :session-running (if the previous act is alive)
or :player-stuck / :sampler-stuck"
  (if (session-alive-p)
      :session-running
      (begin-act act sinks)))

(defun begin-act (act sinks)
  "stop the player and the last recording. Open ACT only once both old
sources hav ejoined"
  (cond ((eq :not-joined (stop-player)) :player-stuck)
        ((eq :sampler-stuck (stop-recording)) :sampler-stuck)
        (t (open-act act sinks))))

(defun open-act (act sinks)
  (reset-between-acts)
  (let ((path (start-recording act)))
    (setf *current-act* act)
    (start-sampler (act-sinks sinks))
    (setf *session-thread*
          (sb-thread:make-thread (lambda () (run-act act)) :name "demo-session"))
    path))

(defun restart-sampler (sinks)
  "Stop the player + samplelr, then sample again into the open recording and SINKS.
Returns what start-sampler returns or :player-stuck / :sampler-stuck"
  (cond ((eq :not-joined (stop-player)) :player-stuck)
        ((eq :not-joined (stop-sampler)) :sampler-stuck)
        (t (start-sampler (act-sinks sinks)))))
