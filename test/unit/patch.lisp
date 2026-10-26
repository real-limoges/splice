;;;; patch.lisp: declaration, the static checks at registration, and the
;;;; contract gate.

(in-package #:splice-tests)

(def-suite patch :in splice-tests)
(in-suite patch)

;;; Functions for the patch sets below to replace. Each test registers its
;;; sets under its own names and drops them afterwards.

(defun probe-one (x) x)
(defun probe-two (a b) (list a b))

;;; Declaration

(test parse-arity
  (flet ((arity (lambda-list) (multiple-value-list (splice::parse-arity lambda-list))))
    (is (equal '(0 0) (arity '())))
    (is (equal '(2 2) (arity '(a b))))
    (is (equal '(1 3) (arity '(a &optional b c))))
    (is (equal '(1 nil) (arity '(a &rest r))))
    (is (equal '(0 nil) (arity '(&key k))))
    (is (equal '(1 nil) (arity '(a &optional b &key k))))
    (is (equal '(1 1) (arity '(a &aux (x 1)))))))

(test defpatch-set-rejects-an-unknown-clause-at-expansion
  (signals error
    (macroexpand-1 '(splice:defpatch-set bogus () (:functoin probe-one (lambda (x) x))))))

(test clause-forms-keep-declaration-order-within-a-kind
  (let ((forms (splice::clause-forms
                :function
                '((:function f1 (lambda ()))
                  (:contract c1 :check (lambda () t))
                  (:function f2 (lambda ()))))))
    (is (equal '('f1 'f2) (mapcar (lambda (form) (getf (rest form) :name)) forms)))))

(test shared-function-names-keeps-the-first-sets-order
  (flet ((set-of (&rest names)
           (splice::make-patch-set
            :fn-patches (mapcar (lambda (n) (splice::make-fn-patch :name n)) names))))
    (is (equal '(c a) (splice::shared-function-names (set-of 'c 'b 'a)
                                                     (set-of 'a 'c 'd))))
    (is (null (splice::shared-function-names (set-of 'a) (set-of 'b))))))

;;; Registration's static checks

(defun register-veto (ps)
  (veto-of (lambda () (splice:register-patch-set ps))))

(test a-compatible-patch-registers
  (with-patch-set-names (arity-ok)
    (let ((ps (fn-patch-set 'arity-ok 'probe-two (lambda (a &optional b c)
                                                   (list a b c)))))
      (is (eq :no-veto (register-veto ps)))
      (is (eq :registered (splice:patch-set-state ps)))
      (is (eq ps (splice:find-patch-set 'arity-ok)))
      (is (eq #'probe-two (splice::fn-patch-old-fn
                           (first (splice::patch-set-fn-patches ps))))))))

(test new-code-requiring-more-arguments-is-vetoed
  (with-patch-set-names (arity-more)
    (let* ((ps (fn-patch-set 'arity-more 'probe-two (lambda (a b c) (list a b c))))
           (veto (register-veto ps)))
      (is (eq :static-arity (splice:patch-vetoed-contract veto)))
      (is (eq :vetoed (splice:patch-set-state ps))))))

(test new-code-accepting-fewer-arguments-is-vetoed
  (with-patch-set-names (arity-fewer)
    (let ((veto (register-veto (fn-patch-set 'arity-fewer 'probe-two
                                             (lambda (a) a)))))
      (is (eq :static-arity (splice:patch-vetoed-contract veto))))))

(test new-code-with-an-unreadable-lambda-list-is-vetoed
  (with-patch-set-names (arity-unknown)
    (let* ((ps (fn-patch-set 'arity-unknown 'probe-two (unreadable-probe)))
           (veto (register-veto ps)))
      (is (eq :static-arity (splice:patch-vetoed-contract veto)))
      (is (eq :vetoed (splice:patch-set-state ps))))))

(test patching-an-unbound-function-is-vetoed
  (with-patch-set-names (missing-fn)
    (let ((veto (register-veto (fn-patch-set 'missing-fn 'no-such-function-anywhere
                                             (lambda (x) x)))))
      (is (eq :static-missing (splice:patch-vetoed-contract veto))))))

(test a-function-in-flight-in-another-set-is-vetoed
  (with-patch-set-names (overlap-first overlap-second)
    (splice:register-patch-set (fn-patch-set 'overlap-first 'probe-one (lambda (x) x)))
    (let ((veto (register-veto (fn-patch-set 'overlap-second 'probe-one
                                             (lambda (x) x)))))
      (is (eq :static-overlap (splice:patch-vetoed-contract veto)))
      (is (search "OVERLAP-FIRST" (splice:patch-vetoed-detail veto))))))

(test a-finished-set-does-not-count-as-in-flight
  (with-patch-set-names (finished-first finished-second)
    (let ((first-set (fn-patch-set 'finished-first 'probe-one (lambda (x) x))))
      (splice:register-patch-set first-set)
      (setf (splice:patch-set-state first-set) :rolled-back)
      (is (eq :no-veto (register-veto (fn-patch-set 'finished-second 'probe-one
                                                    (lambda (x) x))))))))

(test registration-is-audited
  (with-patch-set-names (audited)
    (with-audit-log (path)
      (splice:register-patch-set (fn-patch-set 'audited 'probe-one (lambda (x) x)))
      (let ((entry (find :patch-registered (log-entries path)
                         :key (lambda (e) (getf e :event)))))
        (is (eq 'audited (getf entry :patch-set)))
        (is (equal '(probe-one)
                   (mapcar (lambda (f) (getf f :name)) (getf entry :functions))))))))

(test veto-sets-the-state-audits-and-signals
  (with-audit-log (path)
    (let* ((ps (splice::make-patch-set :name 'veto-probe))
           (veto (veto-of (lambda () (splice::veto ps :static-test "why")))))
      (is (eq :vetoed (splice:patch-set-state ps)))
      (is (eq :static-test (splice:patch-vetoed-contract veto)))
      (is (string= "why" (splice:patch-vetoed-detail veto)))
      (is (eq ps (splice:splice-condition-patch-set veto)))
      (let ((entry (first (log-entries path))))
        (is (eq :contract-result (getf entry :event)))
        (is (eq :veto (getf entry :verdict)))))))

;;; Contracts

(test check-contract-verdicts
  (is (eq :pass (splice::check-contract (contract 'passes (lambda () t)))))
  (is (equal '(:fail nil)
             (multiple-value-list
              (splice::check-contract (contract 'returns-nil (lambda () nil))))))
  (destructuring-bind (verdict detail)
      (multiple-value-list
       (splice::check-contract (contract 'signals (lambda () (error "kaboom")))))
    (is (eq :fail verdict))
    (is (search "kaboom" detail))))

(test new-resolves-the-new-implementation-inside-a-contract
  (let* ((new-fn (lambda (x) (* 2 x)))
         (splice::*contract-context* (fn-patch-set 'new-probe 'probe-one new-fn)))
    (is (eq new-fn (splice:new 'probe-one)))
    (signals error (splice:new 'probe-two))))

(defun registered-with-contracts (name &rest contracts)
  (let ((ps (fn-patch-set name 'probe-one (lambda (x) x) :contracts contracts)))
    (splice:register-patch-set ps)
    ps))

(test passing-contracts-return-t
  (with-patch-set-names (contracts-pass)
    (let ((ps (registered-with-contracts 'contracts-pass
                                         (contract 'one (lambda () t))
                                         (contract 'two (lambda () t)))))
      (is (eq t (splice::run-contracts ps))))))

(test a-failing-pre-flight-contract-vetoes
  (with-patch-set-names (contracts-veto)
    (let* ((ps (registered-with-contracts 'contracts-veto
                                          (contract 'fine (lambda () t))
                                          (contract 'broken (lambda () nil))))
           (veto (veto-of (lambda () (splice::run-contracts ps)))))
      (is (eq 'broken (splice:patch-vetoed-contract veto)))
      (is (eq :vetoed (splice:patch-set-state ps))))))

(test a-failing-runtime-contract-names-itself-without-vetoing
  (with-patch-set-names (contracts-runtime)
    (let ((ps (registered-with-contracts 'contracts-runtime
                                         (contract 'broken (lambda () nil)))))
      (is (eq 'broken (splice::run-contracts ps :phase :runtime)))
      (is (eq :registered (splice:patch-set-state ps))))))

(test contracts-run-outside-any-task
  (with-patch-set-names (contracts-scope)
    (let* ((seen :unset)
           (ps (registered-with-contracts
                'contracts-scope
                (contract 'scope (lambda ()
                                   (setf seen splice::*task-epochs*)
                                   t)))))
      (let ((splice::*task-epochs* (list (list ps :new))))
        (splice::run-contracts ps))
      (is (null seen)))))

;;; T22: a contract veto has to reach the log as a veto, or the set
;;; reconstructs as :registered.

(test a-contract-veto-reconstructs-as-vetoed
  (with-patch-set-names (contracts-logged)
    (with-audit-log (path)
      (let ((ps (registered-with-contracts 'contracts-logged
                                           (contract 'broken (lambda () nil)))))
        (veto-of (lambda () (splice::run-contracts ps)))
        (is (eq :vetoed (cdr (assoc 'contracts-logged
                                    (getf (splice:reconstruct-state path) :sets)))))
        (is (equal '(:veto)
                   (loop for entry in (log-entries path)
                         when (eq :contract-result (getf entry :event))
                           collect (getf entry :verdict))))))))

(test lambda-list-of-agrees-with-sb-introspect
  (let* ((probe (splice::make-patch-set :name 'arity-probe))
         (cases (list #'probe-one
                      #'probe-two
                      (compile nil '(lambda (a &optional b) (list a b)))
                      (funcall (compile nil '(lambda (x) (lambda (a b) (+ a b x)))) 1)
                      #'print-object)))
    (dolist (f cases)
      (is (equal (multiple-value-list
                  (splice::parse-arity (splice::lambda-list-of f)))
                 (multiple-value-list
                  (splice::parse-arity (sb-introspect:function-lambda-list f))))
          "arity of ~a" f))))

(defun unreadable-probe ()
  "A two-argument function whose lambda list (debug 0) leaves unrecorded"
  (compile nil '(lambda (a b)
                 (declare (optimize (debug 0)))
                 (list a b))))

(test lambda-list-of-names-an-unreadable-lambda-list
  (is (eq :unknown (splice::lambda-list-of (unreadable-probe))))
  (is (eq :unknown (splice::lambda-list-of
                    (compile nil '(lambda (a &key k)
                                   (declare (optimize (debug 0)))
                                   (list a k)))))))

(test an-unreadable-lambda-list-gets-a-variadic-shim
  (is (eq :variadic (splice::fixed-arity :unknown)))
  (is (eq :variadic (splice::fixed-arity '(a &optional b))))
  (is (= 2 (splice::fixed-arity '(a b)))))

(test lambda-list-of-reads-through-a-trace
  (unwind-protect
       (progn
         (trace probe-two)
         (is (equal '(a b) (splice::lambda-list-of (fdefinition 'probe-two)))))
    (untrace probe-two)))

()
