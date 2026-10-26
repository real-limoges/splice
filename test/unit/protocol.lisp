;;;; protocol.lisp: the cluster protocol with no transport.
;;;; just the spec hash, the pure conductor step, and one conductor
;;;; and member wired together in this image

(in-package #:splice-tests)

(def-suite cluster-protocol :in splice-tests)
(in-suite cluster-protocol)

(defun wire-value-p (value)
  "True when VALUE holds only keywords, strings, integers, and lists."
  (typecase value
    (null t)
    (keyword t)
    (string t)
    (integer t)
    (cons (and (wire-value-p (car value)) (wire-value-p (cdr value))))
    (t nil)))

(defun protocol-probe (x) x)

(defun probe-spec (&key (ramp '(100))
                     (new-text "(lambda (x) (error \"canary broke on ~a\" x))"))
  "One-function spec replacing PROTOCOL-PROBE in this package. By default
the new code always fails, so the error SPRT rejects at its floor of 5"
  (list :name "PROTOCOL-PROBE-SET" :package "SPLICE-TESTS"
        :description "protocol probe"
        :functions (list (list "PROTOCOL-PROBE" new-text))
        :classes '()
        :contracts '()
        :ramp ramp
        :min-n 5
        :max-n 2000))

(defun shipped-cluster (&rest ids)
  "A cluster of IDS that has shipped the probe spec and the ship's effects"
  (let ((cluster (splice/cluster::empty-cluster 0.01d0 0.05d0)))
    (dolist (id ids)
      (setf cluster (splice/cluster::step-add-member cluster id)))
    (splice/cluster::step-ship cluster
                               (splice/cluster:spec-with-hash (probe-spec)))))

(defun stepped (cluster &rest arrivals)
  "CLUSTER after each (ID MESSAGE) in ARRIVALS, and every effect (in order)"
  (let ((effects '()))
    (dolist (arrival arrivals)
      (multiple-value-bind (next more)
          (splice/cluster::step-receive cluster (first arrival) (second arrival))
        (setf cluster next
              effects (append effects more))))
    (values cluster effects)))

(defun running-cluster (&rest ids)
  "A cluster of IDS whose members have all prepared and commited"
  (apply #'stepped (apply #'shipped-cluster ids)
         (append (mapcar (lambda (id) (list id '(:prepared :name "P"))) ids)
                 (mapcar (lambda (id) (list id '(:committed))) ids))))

(defun sends (effects)
  "The (ID MESSAGE) of every send effect in EFFECTS"
  (loop for effect in effects
        when (eq :send (first effect))
          collect (rest effect)))

(defun audit-kinds (effects)
  (loop for effect in effects
        when (eq :audit (first effect))
          collect (second effect)))

;;; The Spec

(test spec-hash-ignores-printer-settings
  (let ((plain (splice/cluster:spec-hash (probe-spec))))
    (let ((*package* (find-package :splice-tests))
          (*print-case* :downcase)
          (*print-base* 16)
          (*print-radix* t)
          (*print-pretty* t))
      (is (equal plain (splice/cluster:spec-hash (probe-spec)))))))

(test spec-hash-ignores-string-element-type
  (flet ((spec-with-text (element-type)
           (probe-spec :new-text (coerce "(lambda (x) x)"
                                         `(simple-array ,element-type (*))))))
    (is (equal (splice/cluster:spec-hash (spec-with-text 'base-char))
               (splice/cluster:spec-hash (spec-with-text 'character))))))

(test spec-hash-covers-every-field-but-the-hash
  (let ((spec (probe-spec)))
    (is (equal (splice/cluster:spec-hash spec)
               (splice/cluster:spec-hash (splice/cluster:spec-with-hash spec))))
    (is (not (equal (splice/cluster:spec-hash spec)
                    (splice/cluster:spec-hash (probe-spec :ramp '(50 100))))))
    (is (not (equal (splice/cluster:spec-hash spec)
                    (splice/cluster:spec-hash
                     (probe-spec :new-text "(lambda (x) x)")))))))

(test a-tampered-spec-is-refused-before-anything-is-read
  (let ((spec (splice/cluster:spec-with-hash (probe-spec))))
    (setf (getf spec :min-n) 1)
    (unwind-protect
         (progn
           (signals error (splice/cluster:build-patch-set spec))
           (is (null (splice:find-patch-set 'protocol-probe-set))))
      (splice:unregister-patch-set 'protocol-probe-set))))

;;; The Pure Step

(test every-member-prepared-starts-commit
  (multiple-value-bind (cluster effects) (shipped-cluster :a :b)
    (is (eq :preparing (splice/cluster::cluster-phase cluster)))
    (is (equal '(:a :b) (mapcar #'first (sends effects))))
    (is (equal '(:cluster-prepare) (audit-kinds effects)))
    (multiple-value-bind (half none) (stepped cluster '(:a (:prepared :name "P")))
      (is (eq :preparing (splice/cluster::cluster-phase half)))
      (is (null none))
      (multiple-value-bind (both commits) (stepped half '(:b (:prepared :name "P")))
        (is (eq :committing (splice/cluster::cluster-phase both)))
        (is (equal '((:a (:commit)) (:b (:commit))) (sends commits)))))))

(test a-veto-aborts-the-cluster
  (multiple-value-bind (cluster effects)
      (stepped (shipped-cluster :a :b) '(:b (:veto :reason "no")))
    (is (eq :aborted (splice/cluster::cluster-phase cluster)))
    (is (equal '((:a (:bye)) (:b (:bye))) (sends effects)))
    (is (equal '(:cluster-abort) (audit-kinds effects)))))

(test an-abort-during-commit-rolls-back-before-bye
  (multiple-value-bind (committing effects)
      (stepped (shipped-cluster :a :b)
               '(:a (:prepared :name "P"))
               '(:b (:prepared :name "P")))
    (declare (ignore effects))
    (multiple-value-bind (cluster abort)
        (stepped committing '(:b (:veto :reason "contract failed")))
      (is (eq :aborted (splice/cluster::cluster-phase cluster)))
      (is (equal '((:a (:rollback)) (:b (:rollback)) (:a (:bye)) (:b (:bye)))
                 (sends abort))))))

(test the-pooled-llr-rolls-back-once
  (multiple-value-bind (cluster effects)
      (stepped (running-cluster :a :b)
               '(:a (:health :state :canary-5 :fraction 5 :n 200 :llr-milli 2300))
               '(:b (:health :state :canary-5 :fraction 5 :n 200 :llr-milli 2300))
               '(:a (:health :state :canary-5 :fraction 5 :n 400 :llr-milli 3000)))
    (is (eq :running (splice/cluster::cluster-phase cluster)))
    (is (equal '((:a (:rollback)) (:b (:rollback))) (sends effects)))
    (is (= 1 (count :cluster-rollback (audit-kinds effects))))))

(test a-member-rolling-back-rolls-back-the-cluster
  (multiple-value-bind (cluster effects)
      (stepped (running-cluster :a :b)
               '(:a (:health :state :rolling-back :fraction 0 :n 50 :llr-milli 10)))
    (declare (ignore cluster))
    (is (equal '((:a (:rollback)) (:b (:rollback))) (sends effects)))
    (is (eq :member-local-reject
            (getf (cddr (find :audit effects :key #'first)) :trigger)))))

(test a-stalled-member-rolls-back-the-cluster
  (multiple-value-bind (cluster effects)
      (stepped (running-cluster :a :b) '(:a (:stalled :state :canary-25)))
    (is (eq :running (splice/cluster::cluster-phase cluster)))
    (is (equal '((:a (:rollback)) (:b (:rollback))) (sends effects)))
    (is (eq :member-stalled
            (getf (cddr (find :audit effects :key #'first)) :trigger)))))

(test terminal-members-are-asked-for-reports-then-told-bye
  (multiple-value-bind (reporting effects)
      (stepped (running-cluster :a :b)
               '(:a (:terminal :state :promoted))
               '(:b (:terminal :state :promoted)))
    (is (eq :reporting (splice/cluster::cluster-phase reporting)))
    (is (equal '((:a (:report)) (:b (:report))) (sends effects)))
    (multiple-value-bind (done last)
        (stepped reporting
                 '(:b (:final :state :promoted :reversal nil))
                 '(:a (:final :state :promoted :reversal nil)))
      (is (eq :done (splice/cluster::cluster-phase done)))
      (is (equal '((:a (:bye)) (:b (:bye))) (sends last)))
      (is (equal '(:cluster-final) (audit-kinds last)))
      (is (equal '((:member :a :state :promoted :reversal nil)
                   (:member :b :state :promoted :reversal nil))
                 (splice/cluster::cluster-finals done))))))

(test a-gone-member-dies-or-aborts
  (let ((running (stepped (running-cluster :a :b) '(:a (:gone)))))
    (is (equal '(:member :a :state :died)
               (first (splice/cluster::cluster-finals running)))))
  (is (eq :aborted (splice/cluster::cluster-phase
                    (stepped (shipped-cluster :a :b) '(:a (:gone)))))))

(test strangers-change-nothing
  (multiple-value-bind (cluster effects) (shipped-cluster :a)
    (declare (ignore effects))
    (multiple-value-bind (same audits)
        (stepped cluster '(:z (:prepared :name "P")) '(:a (:wibble)))
      (is (eq cluster same))
      (is (equal '(:cluster-unknown-member :cluster-unknown-message)
                 (audit-kinds audits))))))

(test malformed-messages-change-nothing
  (let ((cluster (running-cluster :a :b)))
    (multiple-value-bind (same audits)
        (stepped cluster
                 '(:a :not-a-message)
                 '(:a (:health :state :canary-5 :n 10 :odd))
                 '(:a (:health :state :canary-5 :n 10)))
      (is (eq cluster same))
      (is (equal '(:cluster-bad-message :cluster-bad-message :cluster-bad-message)
                 (audit-kinds audits))))))

(test the-shipped-spec-carries-the-conductors-error-rates
  (let* ((wire (make-test-wire))
         (conductor (splice/cluster:make-conductor :adapter wire
                                                   :alpha 0.001d0 :beta 0.1d0)))
    (splice/cluster:conductor-add-member conductor :m1)
    (splice/cluster:conductor-ship conductor (probe-spec))
    (let ((spec (getf (rest (cdr (wire-take wire))) :spec)))
      (is (= 1000 (getf spec :alpha-ppm)))
      (is (= 100000 (getf spec :beta-ppm)))
      (unwind-protect
           (let ((ps (splice/cluster:build-patch-set spec)))
             (is (= 0.001d0 (splice::patch-set-alpha ps)))
             (is (= 0.1d0 (splice::patch-set-beta ps))))
        (splice:unregister-patch-set 'protocol-probe-set)))))

;;; A conductor and a member in this image, but no socket

(defstruct (test-wire (:constructor make-test-wire ()))
  (lock (sb-thread:make-mutex :name "test wire") :read-only t)
  ;; mutable
  (queue '())
  (seen '()))

(defun wire-post (wire to message)
  (sb-thread:with-mutex ((test-wire-lock wire))
    (setf (test-wire-queue wire)
          (append (test-wire-queue wire) (list (cons to message))))
    (push message (test-wire-seen wire))))

(defun wire-take (wire)
  "The oldest queued (TO . MESSAGE) or NIL"
  (sb-thread:with-mutex ((test-wire-lock wire))
    (pop (test-wire-queue wire))))

(defmethod splice/cluster:send-to-member ((wire test-wire) member-id message)
  (declare (ignore member-id))
  (wire-post wire :member message))

(defmethod splice/cluster:send-to-conductor ((wire test-wire) message)
  (wire-post wire :conductor message))

(defun drive-probe-task (cluster-member id)
  "Run one task over PROTOCOL-PROBE while CLUSTER-MEMBER's set canaries
sending its health every tenth task, otherwise wait a moment"
  (let ((ps (splice/cluster::member-patch-set cluster-member)))
    (if (and ps (splice::canary-stage-p (splice:patch-set-state ps)))
        (progn
          (splice:with-task (:id id :swallow-errors t)
            (funcall 'protocol-probe id))
          (when (zerop (mod id 10))
            (splice/cluster:member-send-health cluster-member)))
        (sleep 0.01))))

(test one-member-runs-in-process
  (let* ((wire (make-test-wire))
         (conductor (splice/cluster:make-conductor :adapter wire))
         (cluster-member (splice/cluster:make-member :adapter wire))
         (deadline (+ (get-internal-real-time)
                      (* 30 internal-time-units-per-second))))
    (unwind-protect
         (progn
           (splice/cluster:conductor-add-member conductor :m1)
           (splice/cluster:conductor-ship conductor (probe-spec))
           (loop for id from 1
                 until (or (member (splice/cluster:conductor-phase conductor)
                                   '(:done :aborted))
                           (> (get-internal-real-time) deadline))
                 do (let ((next (wire-take wire)))
                         (cond ((null next)
                                (drive-probe-task cluster-member id))
                               ((eq :member (car next))
                                (splice/cluster:member-receive cluster-member (cdr next)))
                               (t
                                (splice/cluster:conductor-receive conductor :m1 (cdr next))))))
           (is (eq :done (splice/cluster:conductor-phase conductor)))
           (is (equal '((:member :m1 :state :rolled-back :reversal nil))
                      (splice/cluster:conductor-finals conductor)))
           (is (every #'wire-value-p (test-wire-seen wire))
               "a message held something other than a wire value!"))
      (let ((ps (splice:find-patch-set 'protocol-probe-set)))
        (when ps (retire  ps))))))
