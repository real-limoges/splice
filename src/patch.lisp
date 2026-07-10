;;;; patch.lisp — patch-set declaration, registration (old-world capture,
;;;; static checks), the contract-check veto gate, and shim construction
;;;; (DESIGN.md §4.1, §4.2, §4.5).

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

;;; ---------------------------------------------------------------------
;;; DEFPATCH-SET — the declaration macro

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

;;; ---------------------------------------------------------------------
;;; Static checks (DESIGN.md §4.5 item 4) — run at registration, veto early

(defun parse-arity (lambda-list)
  "(values min-args max-args-or-nil) — NIL max means &rest/&key."
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
  "Proclaimed-inline functions have callers that bypass fdefinition
indirection; a shim would leave stale fast paths (DESIGN.md §11.1)."
  (let ((status (ignore-errors
                 (sb-int:info :function :inlinep (fn-patch-name fp)))))
    (when (member status '(:inline :maybe-inline))
      (veto ps :static-inlining
            (format nil "~a is proclaimed ~a; callers may have inlined it"
                    (fn-patch-name fp) status)))))

(defun check-function-exists (ps fp)
  (unless (fboundp (fn-patch-name fp))
    (veto ps :static-missing
          (format nil "~a is not fbound; cannot patch what isn't there"
                  (fn-patch-name fp)))))

(defun check-overlap (ps)
  "At most one in-flight canary per function (Erlang's two-version cap,
generalized): overlapping live sets are vetoed, not stacked."
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
  ;; static existence + old-world capture first — everything else needs it
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

;;; ---------------------------------------------------------------------
;;; Contracts — the veto gate between REGISTERED and STAGED

(defun run-contracts (ps &key (phase :pre-flight))
  "Run every contract; a failure or error signals PATCH-VETOED at pre-flight
and returns the failing contract at runtime (hard trigger). Returns T when
all pass."
  (let ((*contract-context* ps))
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

;;; ---------------------------------------------------------------------
;;; Shims (DESIGN.md §4.2) — the per-task routing indirection

(defvar *shim-factories* (make-hash-table :test #'eql)
  "Fixed-arity shim factories, compiled once per arity. A fixed-arity shim
avoids &REST/APPLY overhead on the hot path — the difference between ~2x and
~1.5x call cost during canary (criterion §10.7).")

(defun shim-factory (arity)
  "Compiled factory (ps old new) -> shim taking exactly ARITY arguments."
  (or (gethash arity *shim-factories*)
      (setf (gethash arity *shim-factories*)
            (let ((args (loop for i below arity
                              collect (intern (format nil "A~d" i) :splice))))
              (compile nil
                       `(lambda (ps old new)
                          (lambda ,args
                            ;; one assoc does route lookup AND marks the
                            ;; observed route into the pin (coherence trace)
                            (let* ((pin (assoc ps *task-epochs* :test #'eq))
                                   (route (if pin (cadr pin) :old)))
                              (when pin (setf (cddr pin) route))
                              (if (eq route :new)
                                  (if (patch-set-paired-fallback ps)
                                      (handler-case (funcall new ,@args)
                                        ((or error storage-condition) (e)
                                          (note-anomaly ps e)
                                          (funcall old ,@args)))
                                      (handler-bind
                                          (((or error storage-condition)
                                             (lambda (e) (note-anomaly ps e))))
                                        (funcall new ,@args)))
                                  (funcall old ,@args))))))))))

(defun make-shim (ps fp)
  "Dispatch shim for one function: routes on the task's pinned epoch,
captures anomalies out of new code, optionally falls back to old within the
same call (paired fallback). Fixed-arity functions get an APPLY-free shim;
variadic ones fall back to a DYNAMIC-EXTENT &REST version."
  (let ((old (fn-patch-old-fn fp))
        (new (fn-patch-new-fn fp)))
    (multiple-value-bind (min max)
        (parse-arity (sb-introspect:function-lambda-list old))
      (if (and max (= min max))
          (funcall (shim-factory min) ps old new)
          (lambda (&rest args)
            (declare (dynamic-extent args))
            (let ((route (task-route-for ps)))
              (note-shim-route ps route)
              (if (eq route :new)
                  (if (patch-set-paired-fallback ps)
                      (handler-case (apply new args)
                        ((or error storage-condition) (e)
                          (note-anomaly ps e)
                          (apply old args)))
                      (handler-bind (((or error storage-condition)
                                       (lambda (e) (note-anomaly ps e))))
                        (apply new args)))
                  (apply old args))))))))

(defun install-fn-shims (ps)
  (dolist (fp (patch-set-fn-patches ps))
    (let ((shim (make-shim ps fp)))
      (setf (fn-patch-shim fp) shim
            (fdefinition (fn-patch-name fp)) shim))))

(defun remove-fn-shims (ps &key (world :old))
  "Replace shims destructively with one world: :old restores the captured
originals (rollback), :new installs the new implementations (promotion)."
  (dolist (fp (patch-set-fn-patches ps))
    (setf (fdefinition (fn-patch-name fp))
          (ecase world
            (:old (fn-patch-old-fn fp))
            (:new (fn-patch-new-fn fp))))
    (setf (fn-patch-shim fp) nil)))

(defun old-method-callable (mp)
  "A plain function that invokes the captured old method's body."
  (let ((mf (sb-mop:method-function (method-patch-old-method mp))))
    (lambda (&rest args) (funcall mf args nil))))

(defun install-method-shims (ps)
  (dolist (mp (patch-set-method-patches ps))
    (let* ((old-callable (old-method-callable mp))
           (new-fn (method-patch-new-fn mp))
           (arg-names (loop for i from 0
                            below (length (method-patch-specializer-names mp))
                            collect (intern (format nil "A~d" i) :splice)))
           (form `(defmethod ,(method-patch-gf-name mp)
                      ,(mapcar #'list arg-names
                               (method-patch-specializer-names mp))
                    (let ((route (task-route-for ,ps)))
                      (note-shim-route ,ps route)
                      (if (eq route :new)
                          (handler-bind (((or error storage-condition)
                                           (lambda (e) (note-anomaly ,ps e))))
                            (funcall ,new-fn ,@arg-names))
                          (funcall ,old-callable ,@arg-names))))))
      (setf (method-patch-shim-method mp) (eval form)))))

(defun remove-method-shims (ps &key (world :old))
  (dolist (mp (patch-set-method-patches ps))
    (let ((gf (fdefinition (method-patch-gf-name mp))))
      (when (method-patch-shim-method mp)
        (remove-method gf (method-patch-shim-method mp))
        (setf (method-patch-shim-method mp) nil))
      (ecase world
        (:old (add-method gf (method-patch-old-method mp)))
        (:new
         (let ((arg-names (loop for i from 0
                                below (length (method-patch-specializer-names mp))
                                collect (intern (format nil "A~d" i) :splice)))
               (new-fn (method-patch-new-fn mp)))
           (eval `(defmethod ,(method-patch-gf-name mp)
                      ,(mapcar #'list arg-names
                               (method-patch-specializer-names mp))
                    (funcall ,new-fn ,@arg-names)))))))))
