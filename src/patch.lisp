;;;; patch.lisp: patch-set declaration, registration, contracts, and shims.

(in-package #:splice)

(defvar *contract-context* nil
  "Bound to the patch set while its contracts run, so SPLICE:NEW resolves.")

(defun new (name)
  "Inside a contract check: the patch set's new implementation of NAME."
  (let ((fp (find name (patch-set-fn-patches *contract-context*)
                  :key #'fn-patch-name)))
    (unless fp (error "~a is not patched by ~a" name
                      (patch-set-name *contract-context*)))
    (fn-patch-new-fn fp)))

;;; DEFPATCH-SET: the declaration macro

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
  (let ((fns '()) (methods '()) (classes '()) (contracts '()))
    (dolist (clause clauses)
      (ecase (first clause)
        (:function
         (destructuring-bind (fname form) (rest clause)
           (push `(make-fn-patch
                   :name ',fname :new-fn ,form :new-source ',form
                   :new-source-hash (sha256-hex (prin1-to-string ',form)))
                 fns)))
        (:method
            (destructuring-bind ((gf-name &rest specializers) form) (rest clause)
              (push `(make-method-patch
                      :gf-name ',gf-name :specializer-names ',specializers
                      :new-fn ,form :new-source ',form
                      :new-source-hash (sha256-hex (prin1-to-string ',form)))
                    methods)))
        (:class
         (destructuring-bind (cname &key add-slots drop-slots
                                      migrate-forward migrate-backward lossy)
             (rest clause)
           (push `(make-class-patch
                   :class-name ',cname :add-slots ',add-slots
                   :drop-slots ',drop-slots
                   :migrate-forward ,migrate-forward
                   :migrate-backward ,migrate-backward
                   :lossy-p ,lossy)
                 classes)))
        (:contract
         (destructuring-bind (cn &key check) (rest clause)
           (push `(make-contract :name ',cn :check-fn ,check :source ',check)
                 contracts)))))
    `(register-patch-set
      (make-patch-set :name ',name :description ,description :ticket ,ticket
                      :ramp ,ramp :paired-fallback ,paired-fallback
                      :alpha ,alpha :beta ,beta
                      :sprt-min-n ,sprt-min-n :sprt-max-n ,sprt-max-n
                      :fn-patches (list ,@(nreverse fns))
                      :method-patches (list ,@(nreverse methods))
                      :class-patches (list ,@(nreverse classes))
                      :contracts (list ,@(nreverse contracts))))))

;;; Static checks that run at registration, veto early

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
  (multiple-value-bind (old-min old-max)
      (parse-arity (sb-introspect:function-lambda-list (fn-patch-old-fn fp)))
    (multiple-value-bind (new-min new-max)
        (parse-arity (sb-introspect:function-lambda-list (fn-patch-new-fn fp)))
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

(defun check-overlap (ps)
  "At most one in-flight canary per function (Erlang's two-version cap)."
  (maphash
   (lambda (other-name other)
     (unless (or (eq other ps)
                 (member (patch-set-state other)
                         '(:rolled-back :promoted :vetoed)))
       (dolist (fp (patch-set-fn-patches ps))
         (when (find (fn-patch-name fp) (patch-set-fn-patches other)
                     :key #'fn-patch-name)
           (veto ps :static-overlap
                 (format nil "~a is already in flight in set ~a"
                         (fn-patch-name fp) other-name))))))
   *patch-sets*))

(defun veto (ps kind detail)
  (setf (patch-set-state ps) :vetoed)
  (audit :contract-result :patch-set (patch-set-name ps)
                          :contract kind :verdict :veto :detail detail)
  (error 'patch-vetoed :patch-set ps :contract kind :detail detail))

;;; ---------------------------------------------------------------------
;;; Registration: capture the old world, run static checks, audit

(defun register-patch-set (ps)
  (setf (gethash (patch-set-name ps) *patch-sets*) ps)
  ;; static existence + old-world capture first; everything else needs it
  (dolist (fp (patch-set-fn-patches ps))
    (check-function-exists ps fp)
    (setf (fn-patch-old-fn fp) (fdefinition (fn-patch-name fp))
          (fn-patch-old-source-hash fp)
          (sha256-hex (princ-to-string (fdefinition (fn-patch-name fp))))))
  (dolist (mp (patch-set-method-patches ps))
    (let* ((gf (fdefinition (method-patch-gf-name mp)))
           (method (find-method gf (method-patch-qualifiers mp)
                                (mapcar #'find-class
                                        (method-patch-specializer-names mp))
                                nil)))
      (unless method
        (veto ps :static-missing
              (format nil "no method (~a ~{~a~^ ~})"
                      (method-patch-gf-name mp)
                      (method-patch-specializer-names mp))))
      (setf (method-patch-old-method mp) method)))
  (dolist (cp (patch-set-class-patches ps))
    (unless (find-class (class-patch-class-name cp) nil)
      (veto ps :static-missing
            (format nil "class ~a does not exist" (class-patch-class-name cp))))
    (multiple-value-bind (slots supers)
        (capture-class-shape (class-patch-class-name cp))
      (setf (class-patch-old-slot-specs cp) slots
            (class-patch-old-superclass-names cp) supers)))
  ;; remaining static gates
  (check-overlap ps)
  (dolist (fp (patch-set-fn-patches ps))
    (check-arity-compatibility ps fp)
    (check-inlining-hazard ps fp))
  (setf (patch-set-state ps) :registered)
  (push (cons :registered (get-universal-time)) (patch-set-history ps))
  (audit :patch-registered
         :patch-set (patch-set-name ps)
         :description (patch-set-description ps)
         :ticket (patch-set-ticket ps)
         :ramp (patch-set-ramp ps)
         :paired-fallback (patch-set-paired-fallback ps)
         :functions (mapcar (lambda (fp)
                              (list :name (fn-patch-name fp)
                                    :new-source-hash (fn-patch-new-source-hash fp)
                                    :old-identity-hash (fn-patch-old-source-hash fp)
                                    :new-source (fn-patch-new-source fp)))
                            (patch-set-fn-patches ps))
         :methods (mapcar (lambda (mp)
                            (list :gf (method-patch-gf-name mp)
                                  :specializers (method-patch-specializer-names mp)
                                  :new-source-hash (method-patch-new-source-hash mp)))
                          (patch-set-method-patches ps))
         :classes (mapcar #'class-patch-class-name (patch-set-class-patches ps))
         :class-details (mapcar (lambda (cp)
                                  (list :class (class-patch-class-name cp)
                                        :add-slots (class-patch-add-slots cp)
                                        :drop-slots (class-patch-drop-slots cp)
                                        :lossy (class-patch-lossy-p cp)))
                                (patch-set-class-patches ps)))
  ps)

;;; Veto gate between REGISTERED and STAGED

(defun run-contracts (ps &key (phase :pre-flight))
  "Run every contract. A failure signals PATCH-VETOED at pre-flight, or
returns the failing contract's name at runtime. Returns T when all pass."
  (let ((*contract-context* ps)
        (*task-epochs* nil)
        (*task-shim-routes* nil))
    (dolist (contract (patch-set-contracts ps) t)
      (multiple-value-bind (ok err)
          (ignore-errors (values (funcall (contract-check-fn contract))))
        (let ((passed (and ok (not err))))
          (audit :contract-result :patch-set (patch-set-name ps)
                                  :contract (contract-name contract) :phase phase
                                  :verdict (if passed :pass :fail)
                                  :detail (and err (princ-to-string err)))
          (unless passed
            (if (eq phase :pre-flight)
                (progn (setf (patch-set-state ps) :vetoed)
                       (error 'patch-vetoed :patch-set ps
                                            :contract (contract-name contract)
                                            :detail (or (and err (princ-to-string err))
                                                        "check returned NIL")))
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

(declaim (inline record-pinned-route))
(defun record-pinned-route (ps)
  "The task's pinned route for PS, also written onto the pin. :OLD outside a
task. Inline, since it sits on every fixed-arity shim call."
  (let ((pin (assoc ps *task-epochs* :test #'eq)))
    (when pin (setf (cddr pin) (cadr pin)))
    (if pin (cadr pin) :old)))

(defun traced-route (ps)
  "The task's pinned route for PS, added to the task's route trace."
  (let ((route (task-route-for ps)))
    (note-shim-route ps route)
    route))

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

(defvar *shim-factories* (make-hash-table :test #'eql)
  "Shim factories compiled once per arity. Skipping &REST/APPLY cuts canary
call cost from ~2x to ~1.5x.")

(defun fixed-arity-factory-form (arity)
  "Source of a factory (ps old new) -> shim taking exactly ARITY arguments.
The old route is a direct call; closures are built only on the new route."
  (let ((args (arg-names arity)))
    `(lambda (ps old new)
       (lambda ,args
         (if (eq (record-pinned-route ps) :new)
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
      (if (eq (traced-route ps) :new)
          (call-new ps #'run-new #'run-old)
          (run-old)))))

(defun make-shim (ps fp)
  "Shim routing FP's function on the task's pinned route."
  (let ((old (fn-patch-old-fn fp))
        (new (fn-patch-new-fn fp)))
    (multiple-value-bind (min max)
        (parse-arity (sb-introspect:function-lambda-list old))
      (if (and max (= min max))
          (funcall (shim-factory min) ps old new)
          (variadic-shim ps old new)))))

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
       (if (eq (traced-route ,ps) :new)
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
