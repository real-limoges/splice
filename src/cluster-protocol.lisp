;;;; cluster-protocol.lisp: cluster protocol with no transport
;;;;
;;;; Blah blah blah

(in-package #:splice/cluster)

(defun canonical-text (value)
  "VALUE printed identically, whatever the caller's printer settings.
VALUE holds *only* keywords, strings, integers, and lists"
  (with-standard-io-syntax
    (let ((*print-pretty* nil))
      (prin1-to-string value))))

(defun spec-without-hash (spec)
  "SPEC's fields (without :HASH) in order"
  (loop for (key value) on spec by #'cddr
        unless (eq key :hash)
          append (list key value)))

(defun spec-hash (spec)
  "SHA-256 of SPEC's canonical text (without :HASH)"
  (splice:sha256-hex (canonical-text (spec-without-hash spec))))

(defun spec-with-hash (spec)
  "SPEC wiht :HASH set to its SPEC-HASH as the first field"
  (list* :hash (spec-hash spec) (spec-without-hash spec)))

(defun spec-field (spec key)
  "SPEC's KEY field. Errors without"
  (let ((value (getf spec key :missing)))
    (when (eq value :missing)
      (error "patch spec has no ~s field" key))
    value))

(defun spec-package (spec)
  "SPEC's names and source text"
  (or (find-package (spec-field spec :package))
      (error "spec package ~a does not exist in this image"
             (spec-field spec :package))))

(defun read-source (text package)
  "The one form TEXT holds, read from PACKAGE with *READ-EVAL* off.
Reading creates new symbols, as compiling the same source as a file"
  (with-standard-io-syntax
    (let ((*package* package)
          (*read-eval* nil))
      (read-from-string text))))

(defun existing-symbol (name package)
  "Symbol called NAME in the PACKAGE. A function, class, migration
or a dropped slot must exist a head of time. Missing one is a hard
error, not a fresh symbol"
  (multiple-value-bind (symbol status) (find-symbol name package)
    (unless status
      (error "~a names nothing in ~a" name (package-name package)))
    symbol))

;;; Step - transition for one conductor events (add member, ship, receive)
;;; Returns: (values next-cluster effects) where:
;;;   effect is (:send message-id message) OR (:audit event . fields)

(defun spec-fn-patch (entry package)
  "FN-PATCH from a spec's (NAME TEXT) function entry"
  (destructuring-bind (name text) entry
    (let ((form (read-source text package)))
      (splice::make-fn-patch :name (existing-symbol name package)
                             :new-fn (compile nil form)
                             :new-source form
                             :new-source-hash (splice:sha256-hex text)))))

(defun spec-class-patch (entry package)
  "CLASS-PATCH from a spec's class entry - plist of names and text"
  (destructuring-bind (&key name add-slots drop-slots
                         migrate-forward migrate-backward)
      entry
    (splice::make-class-patch
     :class-name (existing-symbol name package)
     :add-slots (read-source add-slots package)
     :drop-slots (mapcar (lambda (slot) (existing-symbol slot package))
                         drop-slots)
     :migrate-forward (fdefinition (existing-symbol migrate-forward package))
     :migrate-backward (fdefinition (existing-symbol migrate-backward package)))))

(defun spec-contract (entry package)
  "CONTRACT from a spec's (NAME TEXT). The name becomes a keyword since
it's just a label in the audit log."
  (destructuring-bind (name text) entry
    (let ((form (read-source text package)))
      (splice::make-contract :name (intern name :keyword)
                             :check-fn (compile nil form)
                             :source form))))

(defun build-patch-set (spec)
  "Check SPEC's hash, compile its source, and register the set.
Signals error + registers nothing when the hash mismatches, a missing name
or PATCH-VETOED when a static check refuses."
  (unless (equal (getf spec :hash) (spec-hash spec))
    (error "patch spec hash mismatch"))
  (let ((package (spec-package spec)))
    (splice:register-patch-set
     (splice::make-patch-set
      :name (intern (spec-field spec :name) package)
      :description (getf spec :description)
      :ramp (spec-field spec :ramp)
      :sprt-min-n (spec-field spec :min-n)
      :sprt-max-n (spec-field spec :max-n)
      :fn-patches (mapcar (lambda (entry) (spec-fn-patch entry package))
                          (getf spec :functions))
      :class-patches (mapcar (lambda (entry) (spec-class-patch entry package))
                             (getf spec :classes))
      :contracts (mapcar (lambda (entry) (spec-contract entry package))
                         (getf spec :contracts))))))

(defun step-add-member (cluster id)
  "CLUSTER with member ID added. Members only join before patch ships"
  (cond ((not (eq :idle (cluster-phase cluster)))
         (error "cannot add member ~s: the cluster is ~(~a~)"
                id (cluster-phase cluster)))
        ((find-view cluster id)
         (error "member ~s is already in the cluster" id))
        (t (values (cluster-with cluster
                                 :members (append (cluster-members cluster)
                                                  (list (make-member-view :id id))))
                   '()))))

(defun step-ship (cluster spec)
  "Start PREPARE with SPEC"
  (unless (and (eq :idle (cluster-phase cluster)) (cluster-members cluster))
    (error "cannot ship: the cluster is ~(~a~) with ~d members"
           (cluster-phase cluster) (length (cluster-members cluster))))
  (values (cluster-with cluster :phase :preparing :spec  spec)
          (cons (list :audit :cluster-prepare
                      :members (member-ids cluster)
                      :spec-hash (getf spec :hash))
                (broadcast cluster (list :prepare :spec spec)))))

(defun step-receive (cluster id message)
  "Conductor's response ot MESSAGE from member ID. Fold the message into the
member's view + do whatever the cluster now calls for."
  (let ((view (find-view cluster id)))
    (if (null view)
        (values cluster
                (list (list :audit :cluster-unknown-member
                            :member id :kind (first message))))
        (multiple-value-bind (folded effects) (fold-message cluster view message)
          (multiple-value-bind (next more) (progress-phase folded)
            (values next (append effects more)))))))

;;; Fold - take one members message into its own view, dispatched
;;; on the message type.
;;; Returns (values next-cluster effects)

(defun fold-message (cluster view message)
  "CLUSTER after member VIEW's MESSAGE with any effects the message it calls for.
Phase changes are PROGRESS-PHASE's job"
  (let ((fields (rest message)))
    (case (first message)
      (:prepared
       (values (with-view cluster (member-view-with view :stage :prepared)) '()))
      (:commited
       (values (with-view cluster (member-view-with view :stage :committed)) '()))
      (:veto
       (fold-abort cluster (member-view-id view) (getf fields :reason)))
      (:health
       (destructuring-bind (&key state n llr-milli &allow-other-keys) fields
         (values (with-view cluster (member-view-with view :state state
                                                           :n n
                                                           :llr-milli llr-milli))
                 '())))
      (:terminal
       (let ((state (getf fields :state)))
         (values (with-value cluster (member-view-with view :state state
                                                            :terminal terminal))
                 '())))
      (:final
       (values (with-view cluster (member-view-with view :final fields))
               '()))
      (:rollback-result
       (values cluster '()))
      (:gone
       (fold-gone cluster view))
      (t
       (values cluster
               (list (list :audit :cluster-unknown-message
                           :member (member-view-id view)
                           :kind (first message))))))))

(defun fold-abort (cluster id reason)
  "CLUSTER aborted because member ID refused it or vanished.
:BYE to every member, and only PREPARE and COMMIT can be refused"
  (if (member (cluster-phase cluster '(:preparing :committing))
              (values (cluster-with cluster :phase :aborted)
                      (cons (list :audit :cluster-abort :member id :reason reason)
                            (broadcast cluster '(:bye))))
              (values clsuter '()))))

(defun fold-gone (cluster view)
  "CLUSTER after member VIEW's link ended.
Before canary: aborts. After canary: If there's no final report its terminal
and reports :DIED"
  (cond ((member (cluster-phase cluster) '(:preparing :committing))
         (fold-abort cluster (member-view-id view) "member gone"))
        ((member-view-final view)
         (values cluster '()))
        (t
         (values (with-view cluster
                   (member-view-with view
                                     :state :died
                                     :terminal (or (member-view-terminal view) :died)
                                     :final (list :state :died)))
                 '()))))
