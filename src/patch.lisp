;;;; patch.lisp: patch-set declaration, registration, contracts, and shims.

(in-package #:splice)

(defvar *contract-context* nil
  "Bound to the patch set while its contracts run, so SPLICE:NEW resolves.")

(defvar *shim-factories* (make-hash-table :test #'eql)
  "Shim factories compiled once per arity. Skipping &REST/APPLY cuts canary
call cost from ~2x to ~1.5x.")

(defun new (name)
  "Inside a contract check: the patch set's new implementation of NAME."
  (let ((fp (find name (patch-set-fn-patches *contract-context*)
                  :key #'fn-patch-name)))
    (unless fp (error "~a is not patched by ~a" name
                      (patch-set-name *contract-context*)))
    (fn-patch-new-fn fp)))

;;; DEFPATCH-SET: the declaration macro
;;;
;;; Each clause kind has a function that turns the clause into the form
;;; constructing its patch struct; the macro collects them by kind.

(defun source-hash-form (form)
  "A form computing FORM's source hash at load time."
  `(sha256-hex (prin1-to-string ',form)))

(defun function-clause-form (fname form)
  `(make-fn-patch :name ',fname :new-fn ,form :new-source ',form
                  :new-source-hash ,(source-hash-form form)))

(defun method-clause-form (signature form)
  (destructuring-bind (gf-name &rest specializers) signature
    `(make-method-patch :gf-name ',gf-name :specializer-names ',specializers
                        :new-fn ,form :new-source ',form
                        :new-source-hash ,(source-hash-form form))))

(defun class-clause-form (cname &key add-slots drop-slots
                                  migrate-forward migrate-backward lossy)
  `(make-class-patch :class-name ',cname
                     :add-slots ',add-slots :drop-slots ',drop-slots
                     :migrate-forward ,migrate-forward
                     :migrate-backward ,migrate-backward
                     :lossy-p ,lossy))

(defun contract-clause-form (cname &key check)
  `(make-contract :name ',cname :check-fn ,check :source ',check))

(defun check-clause-kind (clause)
  "Signal a CASE-FAILURE for a clause kind DEFPATCH-SET does not know."
  (ecase (first clause)
    ((:function :method :class :contract) clause)))

(defun clause-forms (kind clauses)
  "Constructor forms for the CLAUSES of KIND, in declaration order."
  (loop for (clause-kind . body) in clauses
        when (eq clause-kind kind)
          collect (apply (ecase kind
                           (:function #'function-clause-form)
                           (:method #'method-clause-form)
                           (:class #'class-clause-form)
                           (:contract #'contract-clause-form))
                         body)))

(defmacro defpatch-set (name (&key description ticket (ramp ''(5 25 100))
                                paired-fallback (alpha 0.01d0) (beta 0.05d0)
                                (sprt-min-n 50) (sprt-max-n 2000))
                        &body clauses)
  "Declare a coherent patch set. Clauses:
  (:function name lambda-form)
  (:method (gf-name specializer-class-name...) lambda-form)
  (:class name :add-slots (...) :drop-slots (...)
          :migrate-forward lambda :migrate-backward lambda :lossy bool)
  (:contract name :check lambda-form)"
  (mapc #'check-clause-kind clauses)
  `(register-patch-set
    (make-patch-set :name ',name :description ,description :ticket ,ticket
                    :ramp ,ramp :paired-fallback ,paired-fallback
                    :alpha ,alpha :beta ,beta
                    :sprt-min-n ,sprt-min-n :sprt-max-n ,sprt-max-n
                    :fn-patches (list ,@(clause-forms :function clauses))
                    :method-patches (list ,@(clause-forms :method clauses))
                    :class-patches (list ,@(clause-forms :class clauses))
                    :contracts (list ,@(clause-forms :contract clauses)))))

;;; Static checks that run at registration, veto early

(defun lambda-list-of (function)
  "FUNCTION's lambda list. A generic function answers with declared lambda list.
A traced function answers with the function under the trace."
  (let ((lambda-list
          (if (typep function 'generic-function)
              (sb-mop:generic-function-lambda-list function)
              (sb-kernel:%fun-lambda-list (sb-ext:unencapsulated-function function)))))
    (if (listp lambda-list) lambda-list :unknown)))

(defun parse-arity (lambda-list)
  "(values min-args max-args-or-nil). NIL max means &rest/&key.
This is such a cool use of code as data."
  (let ((min 0) (max 0) (unbounded nil) (state :required))
    (dolist (item lambda-list)
      (case item
        (&optional (setf state :optional))
        ((&rest &key &allow-other-keys) (setf unbounded t state :rest))
        (&aux (setf state :aux))
        (t (case state
             (:required (incf min) (incf max))
             (:optional (incf max))))))
    (values min (unless unbounded max))))

(defun check-arity-compatibility (ps fp)
  "New implementation must accept every call shape the old one accepted."
  (let ((old (lambda-list-of (fn-patch-old-fn fp)))
        (new (lambda-list-of (fn-patch-new-fn fp))))
    (if (or (eq :unknown old) (eq :unknown new))
        (veto ps :static-arity
              (format nil "~a: the ~a code's lambda list can't be read (compiled with (debug 0)?)"
                      (fn-patch-name fp) (if (eq :unknown old) "old" "new")))
        (check-arity-covers ps fp old new))))

(defun check-arity-covers (ps fp old new)
  "Veto PS unless lambda list NEW accepts every call shape lambda list OLD does"
  (multiple-value-bind (old-min old-max) (parse-arity old)
    (multiple-value-bind (new-min new-max) (parse-arity new)
      (when (or (> new-min old-min)
                (and new-max (or (null old-max) (< new-max old-max))))
        (veto ps :static-arity
              (format nil "~a: old accepts [~a,~a], new only [~a,~a]"
                      (fn-patch-name fp) old-min old-max new-min new-max))))))

(defun check-inlining-hazard (ps fp)
  "Callers may have inlined a proclaimed-inline function, bypassing a shim."
  (let ((status (ignore-errors
                 (sb-int:info :function :inlinep (fn-patch-name fp)))))
    (when (member status '(inline maybe-inline))
      (veto ps :static-inlining
            (format nil "~a is proclaimed ~a; callers may have inlined it"
                    (fn-patch-name fp) status)))))

(defun check-function-exists (ps fp)
  (unless (fboundp (fn-patch-name fp))
    (veto ps :static-missing
          (format nil "~a is not fbound; cannot patch what isn't there"
                  (fn-patch-name fp)))))

(defun finished-p (ps)
  "True once PS can no longer route a call to new code."
  (member (patch-set-state ps) '(:rolled-back :promoted :vetoed)))

(defun other-in-flight-sets (ps)
  "Every registered set other than PS that is not finished."
  (loop for other being the hash-values of *patch-sets*
        unless (or (eq other ps) (finished-p other))
          collect other))

(defun shared-function-names (ps other)
  "Names of the functions both PS and OTHER patch, in PS's order."
  (let ((theirs (mapcar #'fn-patch-name (patch-set-fn-patches other))))
    (remove-if-not (lambda (name) (member name theirs))
                   (mapcar #'fn-patch-name (patch-set-fn-patches ps)))))

(defun check-overlap (ps)
  "At most one in-flight canary per function (Erlang's two-version cap)."
  (dolist (other (other-in-flight-sets ps))
    (let ((shared (shared-function-names ps other)))
      (when shared
        (veto ps :static-overlap
              (format nil "~a is already in flight in set ~a"
                      (first shared) (patch-set-name other)))))))

(defun veto (ps kind detail)
  (setf (patch-set-state ps) :vetoed)
  (audit :contract-result :patch-set (patch-set-name ps)
                          :contract kind :verdict :veto :detail detail)
  (error 'patch-vetoed :patch-set ps :contract kind :detail detail))

;;; ---------------------------------------------------------------------
;;; Registration: capture the old world, run static checks, audit

(defun capture-old-function (ps fp)
  "Check FP's function exists, then record the original for rollback."
  (check-function-exists ps fp)
  (let ((old (fdefinition (fn-patch-name fp))))
    (setf (fn-patch-old-fn fp) old
          (fn-patch-old-source-hash fp) (sha256-hex (princ-to-string old)))))

(defun find-old-method (mp)
  "The method MP replaces, or NIL when the generic function has none."
  (find-method (fdefinition (method-patch-gf-name mp))
               (method-patch-qualifiers mp)
               (mapcar #'find-class (method-patch-specializer-names mp))
               nil))

(defun capture-old-method (ps mp)
  "Check MP's method exists, then record it for rollback."
  (let ((method (find-old-method mp)))
    (unless method
      (veto ps :static-missing
            (format nil "no method (~a ~{~a~^ ~})"
                    (method-patch-gf-name mp)
                    (method-patch-specializer-names mp))))
    (setf (method-patch-old-method mp) method)))

(defun capture-old-class (ps cp)
  "Check CP's class exists, then record its current shape for rollback."
  (unless (find-class (class-patch-class-name cp) nil)
    (veto ps :static-missing
          (format nil "class ~a does not exist" (class-patch-class-name cp))))
  (multiple-value-bind (slots supers)
      (capture-class-shape (class-patch-class-name cp))
    (setf (class-patch-old-slot-specs cp) slots
          (class-patch-old-superclass-names cp) supers)))

(defun capture-old-world (ps)
  "Record everything PS replaces. The static checks that follow need it."
  (dolist (fp (patch-set-fn-patches ps)) (capture-old-function ps fp))
  (dolist (mp (patch-set-method-patches ps)) (capture-old-method ps mp))
  (dolist (cp (patch-set-class-patches ps)) (capture-old-class ps cp)))

(defun run-static-checks (ps)
  (check-overlap ps)
  (dolist (fp (patch-set-fn-patches ps))
    (check-arity-compatibility ps fp)
    (check-inlining-hazard ps fp)))

(defun fn-patch-audit-fields (fp)
  (list :name (fn-patch-name fp)
        :new-source-hash (fn-patch-new-source-hash fp)
        :old-identity-hash (fn-patch-old-source-hash fp)
        :new-source (fn-patch-new-source fp)))

(defun method-patch-audit-fields (mp)
  (list :gf (method-patch-gf-name mp)
        :specializers (method-patch-specializer-names mp)
        :new-source-hash (method-patch-new-source-hash mp)))

(defun class-patch-audit-fields (cp)
  (list :class (class-patch-class-name cp)
        :add-slots (class-patch-add-slots cp)
        :drop-slots (class-patch-drop-slots cp)
        :lossy (class-patch-lossy-p cp)))

(defun audit-registration (ps)
  (audit :patch-registered
         :patch-set (patch-set-name ps)
         :description (patch-set-description ps)
         :ticket (patch-set-ticket ps)
         :ramp (patch-set-ramp ps)
         :paired-fallback (patch-set-paired-fallback ps)
         :functions (mapcar #'fn-patch-audit-fields (patch-set-fn-patches ps))
         :methods (mapcar #'method-patch-audit-fields (patch-set-method-patches ps))
         :classes (mapcar #'class-patch-class-name (patch-set-class-patches ps))
         :class-details (mapcar #'class-patch-audit-fields
                                (patch-set-class-patches ps))))

(defun register-patch-set (ps)
  "Store PS, capture what it replaces, run the static checks and audit it.
Any failed check vetoes PS."
  (setf (gethash (patch-set-name ps) *patch-sets*) ps)
  (capture-old-world ps)
  (run-static-checks ps)
  (setf (patch-set-state ps) :registered)
  (push (cons :registered (get-universal-time)) (patch-set-history ps))
  (audit-registration ps)
  ps)

;;; Veto gate between REGISTERED and STAGED

(defun check-contract (contract)
  "Run CONTRACT's check. Returns :PASS or :FAIL, and as a second value the
printed error when the check signaled one."
  (multiple-value-bind (ok err)
      (ignore-errors (values (funcall (contract-check-fn contract))))
    (values (if (and ok (not err)) :pass :fail)
            (and err (princ-to-string err)))))

(defun audit-contract-result (ps contract phase verdict detail)
  (audit :contract-result :patch-set (patch-set-name ps)
                          :contract (contract-name contract) :phase phase
                          :verdict verdict :detail detail))

(defun run-contracts (ps &key (phase :pre-flight))
  "Run every contract. A failure signals PATCH-VETOED at pre-flight, or
returns the failing contract's name at runtime. Returns T when all pass.
Contracts run outside any task, so a plain call reaches old code."
  (let ((*contract-context* ps)
        (*task-epochs* nil))
    (dolist (contract (patch-set-contracts ps) t)
      (multiple-value-bind (verdict detail) (check-contract contract)
        (cond ((and (eq verdict :fail) (eq phase :pre-flight))
               (veto ps (contract-name contract) (or detail "check returned NIL")))
              (t
               (audit-contract-result ps contract phase verdict detail)
               (when (eq verdict :fail)
                 (return-from run-contracts (contract-name contract)))))))))

;;; Shims: the per-task routing indirection
;;;
;;; Every shim asks which route its task is pinned to, then either calls old
;;; code or calls new code with its errors noted as canary anomalies. The
;;; three kinds of shim differ only in how they pass arguments, so the shared
;;; steps are ordinary functions and the generated code forwards to them.

(defun arg-names (n)
  "Argument symbols A0 to An-1, for generated lambdas and methods."
  (loop for i below n collect (intern (format nil "A~d" i) :splice)))

(defun call-new-noting-anomalies (ps run-new)
  "Call RUN-NEW. An error or storage condition is noted as a canary anomaly
and keeps unwinding."
  (handler-bind (((or error storage-condition)
                   (lambda (e) (note-anomaly ps e))))
    (funcall run-new)))

(defun call-new-falling-back (ps run-new run-old)
  "Call RUN-NEW. An error or storage condition is noted as a canary anomaly
and answered by calling RUN-OLD instead."
  (handler-case (funcall run-new)
    ((or error storage-condition) (e)
      (note-anomaly ps e)
      (funcall run-old))))

(defun call-new (ps run-new run-old)
  "Run new code for a function shim, falling back to old code on an error
when PS has paired fallback."
  (if (patch-set-paired-fallback ps)
      (call-new-falling-back ps run-new run-old)
      (call-new-noting-anomalies ps run-new)))

;;; Function shims

(defun fixed-arity-factory-form (arity)
  "Source of a factory (ps old new) -> shim taking exactly ARITY arguments.
The old route is a direct call; closures are built only on the new route."
  (let ((args (arg-names arity)))
    `(lambda (ps old new)
       (lambda ,args
         (if (eq (task-route-for ps) :new)
             (flet ((run-new () (funcall new ,@args))
                    (run-old () (funcall old ,@args)))
               (declare (dynamic-extent #'run-new #'run-old))
               (call-new ps #'run-new #'run-old))
             (funcall old ,@args))))))

(defun shim-factory (arity)
  "The compiled factory for ARITY, compiled on first use."
  (or (gethash arity *shim-factories*)
      (setf (gethash arity *shim-factories*)
            (compile nil (fixed-arity-factory-form arity)))))

(defun variadic-shim (ps old new)
  "Shim for a function taking optional, rest or keyword arguments. Every
call goes through &REST and APPLY, so it is slower than a fixed-arity shim."
  (lambda (&rest args)
    (declare (dynamic-extent args))
    (flet ((run-new () (apply new args))
           (run-old () (apply old args)))
      (declare (dynamic-extent #'run-new #'run-old))
      (if (eq (task-route-for ps) :new)
          (call-new ps #'run-new #'run-old)
          (run-old)))))

(defun make-shim (ps fp)
  "Shim routing FP's function on the task's pinned route."
  (let ((old (fn-patch-old-fn fp))
        (new (fn-patch-new-fn fp)))
    (let ((arity (fixed-arity (lambda-list-of old))))
      (if (eq :variadic arity)
          (variadic-shim ps old new)
          (funcall (shim-factory arity) ps old new)))))

(defun fixed-arity (lambda-list)
  "The one argument count LAMBDA-LIST takes, or :VARIADIC when it takes a
range or can't be read"
  (if (eq :unknown lambda-list)
      :variadic
      (multiple-value-bind (min max) (parse-arity lambda-list)
        (if (and max (= min max)) min :variadic))))

(defun install-fn-shims (ps)
  (dolist (fp (patch-set-fn-patches ps))
    (let ((shim (make-shim ps fp)))
      (setf (fn-patch-shim fp) shim
            (fdefinition (fn-patch-name fp)) shim))))

(defun remove-fn-shims (ps &key (world :old))
  "Replace shims with one world: :old for rollback, :new for promotion."
  (dolist (fp (patch-set-fn-patches ps))
    (setf (fdefinition (fn-patch-name fp))
          (ecase world
            (:old (fn-patch-old-fn fp))
            (:new (fn-patch-new-fn fp))))
    (setf (fn-patch-shim fp) nil)))

;;; Method shims

(defun old-method-callable (mp)
  "A function that invokes the captured old method's body."
  (let ((mf (sb-mop:method-function (method-patch-old-method mp))))
    (lambda (&rest args) (funcall mf args nil))))

(defun eval-method-redefinition (form)
  "Evaluate a DEFMETHOD FORM that replaces an existing method on purpose.
SBCL warns on every such redefinition; here it is the intended effect, so
only that warning is muffled."
  (handler-bind ((sb-kernel:redefinition-with-defmethod #'muffle-warning))
    (eval form)))

(defun method-lambda-list (mp args)
  "ARGS paired with MP's specializer class names, for a DEFMETHOD."
  (mapcar #'list args (method-patch-specializer-names mp)))

(defun method-shim-form (ps mp)
  "A DEFMETHOD routing MP's method on the task's pinned route. Method shims
note anomalies from new code but never fall back to old code."
  (let ((args (arg-names (length (method-patch-specializer-names mp))))
        (new (method-patch-new-fn mp))
        (old (old-method-callable mp)))
    `(defmethod ,(method-patch-gf-name mp) ,(method-lambda-list mp args)
       (if (eq (task-route-for ,ps) :new)
           (call-new-noting-anomalies ,ps (lambda () (funcall ,new ,@args)))
           (funcall ,old ,@args)))))

(defun promoted-method-form (mp)
  "A DEFMETHOD whose body is MP's new code, installed at promotion."
  (let ((args (arg-names (length (method-patch-specializer-names mp)))))
    `(defmethod ,(method-patch-gf-name mp) ,(method-lambda-list mp args)
       (funcall ,(method-patch-new-fn mp) ,@args))))

(defun install-method-shims (ps)
  (dolist (mp (patch-set-method-patches ps))
    (setf (method-patch-shim-method mp)
          (eval-method-redefinition (method-shim-form ps mp)))))

(defun remove-method-shim (mp)
  "Take MP's routing method off its generic function, if it is on."
  (when (method-patch-shim-method mp)
    (remove-method (fdefinition (method-patch-gf-name mp))
                   (method-patch-shim-method mp))
    (setf (method-patch-shim-method mp) nil)))

(defun remove-method-shims (ps &key (world :old))
  "Replace method shims with one world: :old for rollback, :new for promotion."
  (dolist (mp (patch-set-method-patches ps))
    (remove-method-shim mp)
    (ecase world
      (:old (add-method (fdefinition (method-patch-gf-name mp))
                        (method-patch-old-method mp)))
      (:new (eval-method-redefinition (promoted-method-form mp))))))
