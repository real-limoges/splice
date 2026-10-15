;;;; lifecycle.lisp: the per-set lock's deferred effects (types.lisp), and
;;;; operator handlers that re-enter the lifecycle from a verdict or a
;;;; rollback signal.

(in-package #:splice-tests)

(def-suite lifecycle :in splice-tests)
(in-suite lifecycle)

;;; WITH-SET-LOCK

(test deferred-effects-run-after-release-in-order
  (let* ((ps (splice::make-patch-set :name 'lock-probe))
         (lock (splice::patch-set-lock ps))
         (seen '()))
    (is (eq :body (splice::with-set-lock (ps)
                    (splice::defer (lambda () (push (list :first (sb-thread:mutex-owner lock))
                                                    seen)))
                    (splice::defer (lambda () (push (list :second (sb-thread:mutex-owner lock))
                                                    seen)))
                    (is (null seen))
                    :body)))
    (is (equal '((:first nil) (:second nil)) (reverse seen)))))

(test defer-outside-the-lock-is-an-error
  (signals unbound-variable (splice::defer (lambda () nil))))

(test a-deferred-effect-can-take-the-lock-again
  (let ((ps (splice::make-patch-set :name 'lock-probe))
        (relocked nil))
    (splice::with-set-lock (ps)
      (splice::defer (lambda ()
                       (splice::with-set-lock (ps) (setf relocked t)))))
    (is-true relocked)))

;;; Live sets. Each test installs a set over LIFECYCLE-PROBE and retires it
;;; afterwards, which puts the original function back.

(defun lifecycle-probe (x) x)

(defun failing-probe-set (name)
  "Every task canaries and every canary fails, so the error SPRT rejects at
its observation floor."
  (let ((ps (fn-patch-set name 'lifecycle-probe
                          (lambda (x) (error "canary broke on ~a" x)))))
    (setf (splice::patch-set-ramp ps) '(100)
          (splice::patch-set-sprt-min-n ps) 5
          (splice::patch-set-runtime-contract-interval ps) 0)
    ps))

(defun healthy-probe-set (name)
  "Two stages, so accepting the first enters the second rather than
starting a promotion thread."
  (let ((ps (fn-patch-set name 'lifecycle-probe (lambda (x) x))))
    (setf (splice::patch-set-ramp ps) '(50 100)
          (splice::patch-set-runtime-contract-interval ps) 0)
    ps))

(defun join-lifecycle-threads (ps)
  "Wait for PS's rollback and promotion threads, if any are running."
  (let ((names (list (format nil "splice-rollback-~a" (patch-set-name ps))
                     (format nil "splice-promote-~a" (patch-set-name ps)))))
    (dolist (thread (sb-thread:list-all-threads))
      (when (member (sb-thread:thread-name thread) names :test #'equal)
        (sb-thread:join-thread thread :default nil)))))

(defun roll-back-for-cleanup (ps)
  (if (eq :frozen (patch-set-state ps))
      (unfreeze ps :rollback :operator "unit-tests")
      (roll-back ps :operator "unit-tests"))
  (wait-for-state ps '(:rolled-back) :timeout 15)
  (join-lifecycle-threads ps))

(defun retire (ps)
  "Restore whatever PS replaced and drop it from the registry."
  (join-lifecycle-threads ps)
  (unless (or (splice::finished-p ps)
              (eq :registered (patch-set-state ps)))
    (roll-back-for-cleanup ps))
  (unregister-patch-set (patch-set-name ps)))

(defmacro with-installed-set ((var form) &body body)
  "Register and install the set FORM makes, bind it to VAR for BODY, and
retire it whatever happened."
  `(let ((,var ,form))
     (unwind-protect (progn (register-patch-set ,var)
                            (install ,var)
                            ,@body)
       (retire ,var))))

(defun drive-tasks (done-p &key (limit 2000))
  "Run probe tasks until DONE-P returns true or LIMIT tasks have run."
  (loop for id from 1 to limit
        until (funcall done-p)
        do (with-task (:id id :swallow-errors t)
             (funcall 'lifecycle-probe id))))

(defun escaped-error (thunk)
  "The error THUNK lets escape, or :NONE when it returns."
  (handler-case (progn (funcall thunk) :none)
    (error (e) e)))

(defun freezing-handler (ps seen)
  "A handler that records its condition in the cons SEEN and freezes PS."
  (lambda (c)
    (push c (car seen))
    (freeze ps "operator handler" :operator "unit-tests")))

(defun state-trail (ps)
  (mapcar #'car (patch-set-lifecycle-history ps)))

;;; Operator handlers re-entering the lifecycle

(test a-verdict-handler-can-freeze-after-an-accept
  (with-installed-set (ps (healthy-probe-set 'accept-then-freeze))
    (let* ((seen (list '()))
           (escaped (escaped-error
                     (lambda ()
                       (handler-bind ((canary-verdict (freezing-handler ps seen)))
                         (drive-tasks (lambda () (car seen))))))))
      (is (eq :none escaped) "~a escaped the task" escaped)
      (is (= 1 (length (car seen))))
      (is (eq :accept (canary-verdict-direction (first (car seen)))))
      (is (search '(:canary-50 :canary-100 :frozen) (state-trail ps))
          "trail was ~s" (state-trail ps))
      (is (eq :frozen (patch-set-state ps)))
      (is (eq :operator (first (splice::patch-set-freeze-reason ps)))))))

(test a-verdict-handler-can-freeze-after-a-reject
  (with-installed-set (ps (failing-probe-set 'reject-then-freeze))
    (let* ((seen (list '()))
           (escaped (escaped-error
                     (lambda ()
                       (handler-bind ((canary-verdict (freezing-handler ps seen)))
                         (drive-tasks (lambda () (car seen))))))))
      (is (eq :none escaped) "~a escaped the task" escaped)
      (is (= 1 (length (car seen))))
      (is (eq :reject (canary-verdict-direction (first (car seen)))))
      ;; the rollback was decided before the handler ran; the final state is
      ;; not asserted, since the rollback thread still runs on the frozen set
      (is (search '(:canary-100 :rolling-back :frozen) (state-trail ps))
          "trail was ~s" (state-trail ps))
      (is-false (splice::canary-stage-p (patch-set-state ps))))))

(test a-rollback-handler-can-freeze-an-operator-rollback
  (with-installed-set (ps (healthy-probe-set 'operator-rollback-then-freeze))
    (let* ((seen (list '()))
           (escaped (escaped-error
                     (lambda ()
                       (handler-bind ((rollback-initiated (freezing-handler ps seen)))
                         (roll-back ps :operator "unit-tests"))))))
      (is (eq :none escaped) "~a escaped roll-back" escaped)
      (is (= 1 (length (car seen))))
      (is (eq :operator (first (rollback-initiated-trigger (first (car seen))))))
      (is (search '(:canary-50 :rolling-back :frozen) (state-trail ps))
          "trail was ~s" (state-trail ps)))))
