;;;; session.lisp: the session driver. Runs the flagship patch set through
;;;; one canary lifecycle under synthetic load and collects the evidence:
;;;; detection, and rollback exactness.

(in-package #:splice-orders)

;;; ---------------------------------------------------------------------
;;; Globals and the structs they hold

(defparameter *worker-prefix* "orders-worker-"
  "Name prefix of RUN-CANARY-SESSION's worker threads.")

(defparameter *thread-prefixes* (list *worker-prefix* "orders-straggler")
  "Every thread-name prefix the example uses, so a viewer can tell its
threads from the rest. orders-straggler is reserved for a held :old task.")

(defstruct pacer
  (pace nil :read-only t)
  (lock (sb-thread:make-mutex :name "orders-pace") :read-only t)
  ;; mutable
  (last-paced nil)
  (paused-at :none))

(defstruct live-session
  (patch-set nil :read-only t)
  (orders nil :read-only t)
  (seed nil :read-only t)
  (fault nil :read-only t)
  (threads nil :read-only t)
  (started nil :read-only t)
  (audit-start-seq nil :read-only t)
  (pacer nil :read-only t))

(defvar *live-session* nil
  "The session RUN-CANARY-SESSION is running or last ran (for other threasd)
NIL from the start of a session until its install succeeds")

(defstruct session-result
  final-state transitions tasks-run veto-p
  detection-observations reversal-report pre-existing-snapshot
  post-rollback-comparison wallclock-ms)

;;; Hooks for observers (live session, pacing, held :old task)

(defun publish-session (&rest initargs)
  "Make a LIVE-SESSION from INITARGS and publish it to every thread"
  (setf *live-session* (apply #'make-live-session initargs)))

(defun await-pace (pacer ps)
  "Hold this worker while the presenter holds a new stage.
Returns :passed or :paced"
  (cond ((null (pacer-pace pacer)) :passed)
        ((eql (splice::patch-set-stage-index ps) (pacer-last-paced pacer)) :passed)
        (t (sb-thread:with-mutex ((pacer-lock pacer))
             (pace-stage pacer (splice::patch-set-stage-index ps))))))

(defun pace-stage (pacer stage)
  "Pace STAGE unless another work already did this while this one waited"
  (if (eql stage (pacer-last-paced pacer))
      :passed
      (progn (setf (pacer-paused-at pacer) stage)
             (unwind-protect (funcall (pacer-pace pacer) stage)
               (setf (pacer-paused-at pacer) :none))
             (setf (pacer-last-paced pacer) stage)
             :paced)))

(defun live-session-paused-at (session)
  "Stage index SESSION's presenter is holding or :NONE"
  (pacer-paused-at (live-session-pacer session)))

(defun old-pinned-task-id (set-name &key (from 1000000))
  "First task id from FROM whose routing draws pins :old at every stage below 100"
  (loop for id from from
        when (= 99 (splice::routing-draw id set-name))
          return id))

(defun start-held-task (ps hold)
  "Start a task pinned :old on PS that calls HOLD. It blocks before it does
its work. PS must be in the canary stage below 100. Returns the thread"
  (let ((id (old-pinned-task-id (splice:patch-set-name ps))))
    (sb-thread:make-thread
     (lambda ()
       (splice:with-task (:id id :swallow-errors t)
         (funcall hold)
         (process-order (make-test-order))))
     :name "orders-straggler")))

;;; ---------------------------------------------------------------------
;;; Session driver

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
  (splice:unregister-patch-set 'currency-migration)
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
                             paired-fallback quiet
                             pace (settle-timeout 30))
  "Run one patch lifecycle under synthetic load; return a SESSION-RESULT.
Expects an open audit log. PACE, when given, is called with each stage index
and blocks until that stage may run. SETTLE-TIMEOUT is how long to wait for a
terminal state once the workers stop."
  ;; withdraw the last session before the workload it describes is reset
  (setf *live-session* nil)
  (reset-workload)
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
         (pacer (make-pacer :pace pace))
         (tasks-run (list 0))
         (stop (list nil)))
    (handler-case (splice:install ps)
      (splice:patch-vetoed (e)
        (return-from run-canary-session
          (make-session-result :final-state :vetoed :veto-p t
                               :transitions (princ-to-string e)))))
    (publish-session :patch-set ps :orders orders :seed seed :fault fault
                     :threads threads :started (get-internal-real-time)
                     :audit-start-seq audit-start-seq :pacer pacer)
    (flet ((run-one (i task-rng)
             (await-pace pacer ps)
             ;; think time keeps load tracking the ramp when canary is slow
             (when (> threads 1) (sleep 0.0002))
             (when (fault-active-p :load-correlated)
               (fault-window-note *fault* :total))
             (splice:with-task (:id i :swallow-errors t)
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
             (member (splice:patch-set-state ps)
                     '(:promoted :rolled-back :frozen :vetoed
                       :promoting :swapping :rolling-back :restoring))))
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
                                    :name (format nil "~a~a" *worker-prefix* w))))))
            (mapc #'sb-thread:join-thread workers))
          (loop for i from 1 to max-tasks
                until (terminal-p)
                do (run-one i rng) (setf (car tasks-run) i))))
    (splice:wait-for-state ps '(:promoted :rolled-back :frozen) :timeout settle-timeout)
    (let ((result
            (make-session-result
             :final-state (splice:patch-set-state ps)
             :transitions (splice:patch-set-lifecycle-history ps)
             :tasks-run (car tasks-run)
             :detection-observations (detection-observations-from-audit audit-start-seq)
             :reversal-report (splice:reverse-migration-report 'currency-migration)
             :pre-existing-snapshot pre-snapshot
             :post-rollback-comparison
             (when (eq (splice:patch-set-state ps) :rolled-back)
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

(defun run-workload (&key (tasks 500) (seed 1))
  "Plain workload with no patch in flight (baseline / overhead measurement)."
  (let ((rng (make-rng seed)))
    (loop for i from 1 to tasks
          do (splice:with-task (:id i :swallow-errors t)
               (process-order (make-instance 'order
                                             :id i :item-count (rng-range rng 1 9)
                                             :unit-price (/ (rng-range rng 100 9999) 100.0)
                                             :discount-code (case (mod (rng-next rng) 5)
                                                              (0 "TEN") (1 "VIP") (t nil))))))))
