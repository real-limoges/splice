;;;; lifecycle.lisp: the per-set lock's deferred effects (types.lisp),
;;;; operator handlers that re-enter the lifecycle from a verdict or a
;;;; rollback signal, and which operator requests each state allows.

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
  (let ((names (list (format nil "splice-rollback-~a" (splice:patch-set-name ps))
                     (format nil "splice-promote-~a" (splice:patch-set-name ps)))))
    (dolist (thread (sb-thread:list-all-threads))
      (when (member (sb-thread:thread-name thread) names :test #'equal)
        (sb-thread:join-thread thread :default nil)))))

(defun roll-back-for-cleanup (ps)
  (if (eq :frozen (splice:patch-set-state ps))
      (splice:unfreeze ps :rollback :operator "unit-tests")
      (splice:roll-back ps :operator "unit-tests"))
  (splice:wait-for-state ps '(:rolled-back) :timeout 15)
  (join-lifecycle-threads ps))

(defun retire (ps)
  "Restore whatever PS replaced and drop it from the registry."
  (join-lifecycle-threads ps)
  (unless (or (splice::finished-p ps)
              (eq :registered (splice:patch-set-state ps)))
    (roll-back-for-cleanup ps))
  (splice:unregister-patch-set (splice:patch-set-name ps)))

(defmacro with-installed-set ((var form) &body body)
  "Register and install the set FORM makes, bind it to VAR for BODY, and
retire it whatever happened."
  `(let ((,var ,form))
     (unwind-protect (progn (splice:register-patch-set ,var)
                            (splice:install ,var)
                            ,@body)
       (retire ,var))))

(defun drive-tasks (done-p &key (limit 2000))
  "Run probe tasks until DONE-P returns true or LIMIT tasks have run."
  (loop for id from 1 to limit
        until (funcall done-p)
        do (splice:with-task (:id id :swallow-errors t)
             (funcall 'lifecycle-probe id))))

(defun escaped-error (thunk)
  "The error THUNK lets escape, or :NONE when it returns."
  (handler-case (progn (funcall thunk) :none)
    (error (e) e)))

(defun freezing-handler (ps seen)
  "A handler that records its condition in the cons SEEN and freezes PS."
  (lambda (c)
    (push c (car seen))
    (splice:freeze ps "operator handler" :operator "unit-tests")))

(defun state-trail (ps)
  (mapcar #'car (splice:patch-set-lifecycle-history ps)))

;;; Operator handlers re-entering the lifecycle

(test a-verdict-handler-can-freeze-after-an-accept
  (with-installed-set (ps (healthy-probe-set 'accept-then-freeze))
    (let* ((seen (list '()))
           (escaped (escaped-error
                     (lambda ()
                       (handler-bind ((splice:canary-verdict (freezing-handler ps seen)))
                         (drive-tasks (lambda () (car seen))))))))
      (is (eq :none escaped) "~a escaped the task" escaped)
      (is (= 1 (length (car seen))))
      (is (eq :accept (splice:canary-verdict-direction (first (car seen)))))
      (is (search '(:canary-50 :canary-100 :frozen) (state-trail ps))
          "trail was ~s" (state-trail ps))
      (is (eq :frozen (splice:patch-set-state ps)))
      (is (eq :operator (first (splice::patch-set-freeze-reason ps)))))))

(test a-verdict-handler-can-freeze-after-a-reject
  (with-installed-set (ps (failing-probe-set 'reject-then-freeze))
    (let* ((seen (list '()))
           (escaped (escaped-error
                     (lambda ()
                       (handler-bind ((splice:canary-verdict (freezing-handler ps seen)))
                         (drive-tasks (lambda () (car seen))))))))
      (is (eq :none escaped) "~a escaped the task" escaped)
      (is (= 1 (length (car seen))))
      (is (eq :reject (splice:canary-verdict-direction (first (car seen)))))
      ;; the rollback was decided before the handler ran, and its thread
      ;; gives way to the freeze once the drain ends
      (join-lifecycle-threads ps)
      (is (search '(:canary-100 :rolling-back :frozen) (state-trail ps))
          "trail was ~s" (state-trail ps))
      (is (eq :frozen (splice:patch-set-state ps))))))

(test a-rollback-handler-can-freeze-an-operator-rollback
  (with-installed-set (ps (healthy-probe-set 'operator-rollback-then-freeze))
    (let* ((seen (list '()))
           (escaped (escaped-error
                     (lambda ()
                       (handler-bind ((splice:rollback-initiated (freezing-handler ps seen)))
                         (splice:roll-back ps :operator "unit-tests"))))))
      (is (eq :none escaped) "~a escaped roll-back" escaped)
      (is (= 1 (length (car seen))))
      (is (eq :operator (first (splice:rollback-initiated-trigger (first (car seen))))))
      (join-lifecycle-threads ps)
      (is (search '(:canary-50 :rolling-back :frozen) (state-trail ps))
          "trail was ~s" (state-trail ps))
      (is (eq :frozen (splice:patch-set-state ps))))))

(test a-verdict-handler-that-rolls-back-starts-one-rollback
  (with-installed-set (ps (failing-probe-set 'reject-then-roll-back))
    (let* ((returned (list '()))
           (escaped (escaped-error
                     (lambda ()
                       (handler-bind ((splice:canary-verdict
                                        (lambda (c)
                                          (declare (ignore c))
                                          (push (splice:roll-back ps :operator "unit-tests")
                                                (car returned)))))
                         (drive-tasks (lambda () (car returned))))))))
      (is (eq :none escaped) "~a escaped the task" escaped)
      (is (equal '(:no-change) (car returned)))
      (is (eq :rolled-back (splice:wait-for-state ps '(:rolled-back) :timeout 15)))
      (join-lifecycle-threads ps)
      (is (= 1 (count :rolling-back (state-trail ps)))
          "trail was ~s" (state-trail ps))
      (is (= 1 (count :restoring (state-trail ps)))
          "trail was ~s" (state-trail ps)))))

(test a-sealed-set-cannot-be-rolled-back
  (let ((original (fdefinition 'lifecycle-probe)))
    (unwind-protect
         (with-installed-set (ps (healthy-probe-set 'promote-then-seal))
           (is (eq :promoting (splice:promote ps :operator "unit-tests")))
           (is (eq :promoted (splice:wait-for-state ps '(:promoted) :timeout 15)))
           (join-lifecycle-threads ps)
           (is (equal '(:canary-50 :promoting :swapping :promoted)
                      (last (state-trail ps) 4))
               "trail was ~s" (state-trail ps))
           (is (eq :sealed (splice:seal-patch ps :operator "unit-tests")))
           (signals splice:illegal-transition (splice:roll-back ps :operator "unit-tests"))
           (is (eq :promoted (splice:patch-set-state ps))))
      (setf (fdefinition 'lifecycle-probe) original))))

(test sealing-a-canarying-set-is-refused
  (with-installed-set (ps (healthy-probe-set 'seal-mid-canary))
    (signals splice:illegal-transition (splice:seal-patch ps :operator "unit-tests"))
    (is-false (splice::patch-set-sealed ps))
    (is (eq :canary-50 (splice:patch-set-state ps)))))

;;; Which operator requests each state allows, on bare sets: none of these
;;; reach a transition, so nothing needs installing

(defun bare-set (state &rest initargs)
  (apply #'splice::make-patch-set :name 'bare :state state initargs))

(defun outcome-of (thunk)
  "THUNK's value, or :REFUSED when it signals ILLEGAL-TRANSITION."
  (handler-case (funcall thunk)
    (splice:illegal-transition () :refused)))

(test operator-requests-on-each-state
  (flet ((check (request states expected)
           (dolist (state states)
             (let ((ps (bare-set state)))
               (is (eq expected (outcome-of (lambda () (funcall request ps))))
                   "~a from ~a" request state)
               (is (eq state (splice:patch-set-state ps)))))))
    (let ((freeze-it (lambda (ps) (splice:freeze ps "probe" :operator "unit-tests")))
          (roll-it-back (lambda (ps) (splice:roll-back ps :operator "unit-tests")))
          (promote-it (lambda (ps) (splice:promote ps :operator "unit-tests")))
          (seal-it (lambda (ps) (splice:seal-patch ps :operator "unit-tests"))))
      (check freeze-it '(:frozen) :no-change)
      (check freeze-it '(:registered :restoring :swapping :rolled-back :promoted :vetoed)
             :refused)
      (check roll-it-back '(:rolling-back :restoring :rolled-back) :no-change)
      (check roll-it-back '(:registered :promoting :swapping :promoted :vetoed) :refused)
      (check promote-it '(:promoting :swapping :promoted) :no-change)
      (check promote-it '(:registered :staged :rolling-back :restoring :rolled-back :vetoed)
             :refused)
      (check seal-it '(:registered :canary-5 :frozen :rolling-back :rolled-back) :refused))))

;;; Completion threads give way to a freeze that lands during the drain.
;;; Nothing is pinned, so the drain ends at once.

(test a-rollback-thread-leaves-a-frozen-set-alone
  (let ((ps (bare-set :frozen)))
    (is (eq :abandoned (splice::complete-rollback ps)))
    (is (eq :frozen (splice:patch-set-state ps)))))

(test a-promotion-thread-leaves-a-frozen-set-alone
  (let ((ps (bare-set :frozen)))
    (is (eq :abandoned (splice::complete-promotion ps)))
    (is (eq :frozen (splice:patch-set-state ps)))))

;;; Promotion swaps only after the :old pins drain (decision 0008). A bare
;;; set has no shims, so these exercise the drain and the claim alone.

(test a-drain-timeout-freezes-instead-of-swapping
  (let ((ps (bare-set :promoting :pinned-old 1))
        (splice::*drain-timeout-seconds* 0.05))
    (is (eq :frozen (splice::complete-promotion ps)))
    (is (eq :frozen (splice:patch-set-state ps)))
    (is (eq :promotion-drain-timeout (first (splice::patch-set-freeze-reason ps))))
    (is (equal '(:frozen) (state-trail ps)) "trail was ~s" (state-trail ps))))

(test a-drain-that-completes-swaps
  (let ((ps (bare-set :promoting)))
    (is (eq :promoted (splice::complete-promotion ps)))
    (is (equal '(:swapping :promoted) (state-trail ps)) "trail was ~s" (state-trail ps))))
