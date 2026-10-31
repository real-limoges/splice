;;;; migration.lisp: reversible class redefinition.
;;;; During canary the class has the UNION shape (old + new slots) so old code
;;;; stays correct; promotion applies the final shape, rollback the original.
;;;; Forward migration journals each instance's pre-state for an exact
;;;; reversal. Canary-born instances get author-defined backward synthesis.

(in-package #:splice)

(defvar *migration-registry* (make-hash-table :test #'eq)
  "class-name -> (class-patch . phase), phase in (:forward :backward :finalize)
Consulted by the global UPDATE-INSTANCE-FOR-REDEFINED-CLASS method.")

(defvar *migration-journal* (make-hash-table :test #'eq)
  "instance -> (:set name :seq n :values pre-migration-slot-plist).
Strong keys, because SBCL 2.2.9 weak tables corrupt (\"Corrupt NEXT-chain\")
under GC pressure even when mutex-guarded. Entries are purged at rollback, or
at SEAL-PATCH after promotion. Guarded by *MIGRATION-LOCK*.")

(defvar *canary-population* (make-hash-table :test #'eq)
  "instance -> (class-name . creator-route), for tracked instances created
during a canary. With no pre-state to restore, reversal synthesizes the ones
new code made and leaves the old-code ones alone. Strong keys, as above.")

(defvar *tracked-classes* (make-hash-table :test #'eq)
  "class-name -> patch-set, while a class patch is in flight.")

(defvar *journal-seq* 0)
(defvar *migration-lock* (sb-thread:make-mutex))

;;; ---------------------------------------------------------------------
;;; Capturing and rebuilding class shapes via the MOP

(defun canonicalize-slot-spec (spec)
  "defclass-style slot spec -> ensure-class canonical plist."
  (destructuring-bind (name &rest opts) (if (listp spec) spec (list spec))
    (let ((initform nil) (initfunction nil) (initargs '())
          (readers '()) (writers '()) (type t))
      (loop for (key val) on opts by #'cddr
            do (ecase key
                 (:initform (setf initform val
                                  initfunction (compile nil `(lambda () ,val))))
                 (:initarg (push val initargs))
                 (:reader (push val readers))
                 (:writer (push val writers))
                 (:accessor (push val readers)
                  (push `(setf ,val) writers))
                 (:type (setf type val))))
      (list :name name :initform initform :initfunction initfunction
            :initargs (nreverse initargs) :readers (nreverse readers)
            :writers (nreverse writers) :type type))))

(defun capture-class-shape (class-name)
  "Current direct-slot specs and superclass names: what rollback restores."
  (let ((class (find-class class-name)))
    (sb-mop:finalize-inheritance class)
    (values
     (mapcar (lambda (sd)
               (list :name (sb-mop:slot-definition-name sd)
                     :initform (sb-mop:slot-definition-initform sd)
                     :initfunction (sb-mop:slot-definition-initfunction sd)
                     :initargs (sb-mop:slot-definition-initargs sd)
                     :readers (sb-mop:slot-definition-readers sd)
                     :writers (sb-mop:slot-definition-writers sd)
                     :type (sb-mop:slot-definition-type sd)))
             (sb-mop:class-direct-slots class))
     (mapcar #'class-name (sb-mop:class-direct-superclasses class)))))

(defun apply-class-shape (class-name slot-specs superclass-names)
  (sb-mop:ensure-class class-name
                       :direct-superclasses superclass-names
                       :direct-slots slot-specs
                       :metaclass (class-of (find-class class-name))))

(defun class-patch-union-slots (cp)
  "Old slots plus added ones; dropped slots stay until promotion."
  (append (class-patch-old-slot-specs cp)
          (remove-if (lambda (spec)
                       (find (getf spec :name)
                             (class-patch-old-slot-specs cp)
                             :key (lambda (s) (getf s :name))))
                     (mapcar #'canonicalize-slot-spec
                             (class-patch-add-slots cp)))))

(defun class-patch-final-slots (cp)
  "Old slots − dropped + added: the shape promotion makes permanent."
  (append (remove-if (lambda (spec)
                       (member (getf spec :name) (class-patch-drop-slots cp)))
                     (class-patch-old-slot-specs cp))
          (remove-if (lambda (spec)
                       (find (getf spec :name)
                             (class-patch-old-slot-specs cp)
                             :key (lambda (s) (getf s :name))))
                     (mapcar #'canonicalize-slot-spec
                             (class-patch-add-slots cp)))))

(defun snapshot-slots (instance slot-specs)
  "Plist of (slot-name value) for every bound slot in SLOT-SPECS."
  (loop for spec in slot-specs
        for name = (getf spec :name)
        when (slot-boundp instance name)
          append (list name (slot-value instance name))))

;;; ---------------------------------------------------------------------
;;; Instance migration
;;; One :AFTER method on STANDARD-OBJECT consults the registry, instead of
;;; generating a method per class. Untracked classes pay one hash lookup.

(defmethod update-instance-for-redefined-class :after
    ((instance standard-object) added-slots discarded-slots property-list
     &rest initargs)
  (declare (ignore initargs))
  (let ((entry (gethash (class-name (class-of instance)) *migration-registry*)))
    (when entry
      (destructuring-bind (cp . phase) entry
        (ecase phase
          (:forward (migrate-instance-forward cp instance))
          (:backward (migrate-instance-backward cp instance discarded-slots
                                                property-list))
          (:finalize
           (audit :instance-migration :direction :finalize
                                      :class (class-name (class-of instance))
                                      :discarded (mapcar (lambda (s) s) discarded-slots))))))))

(defun migrate-instance-forward (cp instance)
  "Journal the pre-migration state, then run the author's forward migration.
Any error is a MIGRATION-FAILURE, a hard rollback trigger."
  (let ((pre-state (snapshot-slots instance (class-patch-old-slot-specs cp)))
        (seq nil))
    (sb-thread:with-mutex (*migration-lock*)
      (setf seq (incf *journal-seq*)
            (gethash instance *migration-journal*)
            (list :set (class-patch-class-name cp) :seq seq :values pre-state)))
    (handler-case
        (progn
          (when (class-patch-migrate-forward cp)
            (funcall (class-patch-migrate-forward cp) instance pre-state))
          (audit :instance-migration :direction :forward
                                     :class (class-patch-class-name cp) :journal-seq seq
                                     :pre-state pre-state))
      (error (e)
        (audit :instance-migration :direction :forward
                                   :class (class-patch-class-name cp) :journal-seq seq
                                   :outcome :failed :cause (princ-to-string e))
        ;; arm the hard trigger directly: an old-routed task may have fired
        ;; this, and no shim would see it
        (let ((ps (gethash (class-patch-class-name cp) *tracked-classes*)))
          (when ps
            (arm-hard-trigger ps (list :migration-failure (princ-to-string e)))))
        (error 'migration-failure :instance instance :direction :forward
                                  :cause e)))))

(defun migrate-instance-backward (cp instance discarded-slots property-list)
  "Restore from the journal when possible. Otherwise the instance was
canary-born, and the author's backward migration synthesizes old values."
  (declare (ignore discarded-slots))
  (multiple-value-bind (record birth)
      (sb-thread:with-mutex (*migration-lock*)
        (values (gethash instance *migration-journal*)
                (gethash instance *canary-population*)))
    (handler-case
        (cond
          (record
           (loop for (name value) on (getf record :values) by #'cddr
                 ;; only restore slots that exist in the reverted shape
                 when (slot-exists-p instance name)
                   do (setf (slot-value instance name) value))
           (audit :instance-migration :direction :backward :mode :journal-exact
                                      :class (class-patch-class-name cp)
                                      :journal-seq (getf record :seq))
           :journal-exact)
          ((eq (cdr birth) :old)
           ;; made by old code: dropping the added slots is the whole rollback
           (audit :instance-migration :direction :backward
                                      :mode :old-born-unchanged
                                      :class (class-patch-class-name cp))
           :old-born)
          (t
           (when (class-patch-migrate-backward cp)
             (funcall (class-patch-migrate-backward cp) instance property-list))
           (audit :instance-migration :direction :backward
                                      :mode :synthesized-reversal
                                      :class (class-patch-class-name cp))
           :synthesized))
      (migration-failure (e) (error e))
      (error (e)
        (audit :instance-migration :direction :backward :outcome :failed
                                   :class (class-patch-class-name cp) :cause (princ-to-string e))
        (error 'migration-failure :instance instance :direction :backward
                                  :cause e)))))

;;; Track canary-born instances, which never pass through forward migration.
(defmethod initialize-instance :after ((instance standard-object)
                                       &rest initargs)
  (declare (ignore initargs))
  (let* ((name (class-name (class-of instance)))
         (ps (gethash name *tracked-classes*)))
    (when ps
      (let ((route (task-route-for ps)))
        (sb-thread:with-mutex (*migration-lock*)
          (setf (gethash instance *canary-population*) (cons name route)))))))

;;; ---------------------------------------------------------------------
;;; Phase drivers
;;; Called by the lifecycle controller.

(defun begin-class-canary (ps cp)
  "Apply the union shape and start forward migration (lazy, on first touch)."
  (setf (gethash (class-patch-class-name cp) *migration-registry*)
        (cons cp :forward)
        (gethash (class-patch-class-name cp) *tracked-classes*) ps)
  (apply-class-shape (class-patch-class-name cp)
                     (class-patch-union-slots cp)
                     (class-patch-old-superclass-names cp))
  (audit :class-shape :class (class-patch-class-name cp) :shape :union
                      :patch-set (patch-set-name ps)
                      :added (mapcar (lambda (s) (getf (canonicalize-slot-spec s) :name))
                                     (class-patch-add-slots cp))
                      :retained-drops (class-patch-drop-slots cp)))

(defun touch-instance (instance)
  "Force a pending instance update. Only slot access trips the trap."
  (let ((slots (sb-mop:class-slots (class-of instance))))
    (when slots
      (slot-boundp instance (sb-mop:slot-definition-name (first slots))))))

(defun eager-migrate-population (class-name)
  "Migrate now instead of lazily by touching every known instance. Returns
the count touched."
  (let ((instances '()))
    (sb-thread:with-mutex (*migration-lock*)
      (maphash (lambda (inst birth)
                 (when (eq (car birth) class-name) (push inst instances)))
               *canary-population*)
      (maphash (lambda (inst record)
                 (when (eq (getf record :set) class-name)
                   (pushnew inst instances)))
               *migration-journal*))
    (dolist (inst instances)
      (touch-instance inst))
    (length instances)))

(defun instances-to-reverse (class-name)
  "Every journaled or canary-tracked instance of CLASS-NAME, as
(instance . kind) where KIND is :journaled, :canary-born, or :old-born.
An instance in both tables is listed once, as :journaled."
  (sb-thread:with-mutex (*migration-lock*)
    (let ((journaled '()) (born '()))
      (maphash (lambda (inst record)
                 (when (eq (getf record :set) class-name)
                   (push (cons inst :journaled) journaled)))
               *migration-journal*)
      (maphash (lambda (inst birth)
                 (when (and (eq (car birth) class-name)
                            (not (assoc inst journaled :test #'eq)))
                   (push (cons inst (if (eq (cdr birth) :old)
                                        :old-born :canary-born))
                         born)))
               *canary-population*)
      (append born journaled))))

(defun reversal-counts (instances)
  "How many of INSTANCES were reversed each way."
  (flet ((count-kind (kind) (count kind instances :key #'cdr)))
    (list :journal-exact (count-kind :journaled)
          :synthesized (count-kind :canary-born)
          :old-born-unchanged (count-kind :old-born))))

(defun rollback-class-patch (ps cp)
  "Revert the class, then touch every journaled or canary-born instance so
the counts are deterministic. The first instance that fails to migrate back
aborts the rollback. Returns the report plist."
  (let ((class-name (class-patch-class-name cp)))
    (setf (gethash class-name *migration-registry*) (cons cp :backward))
    (apply-class-shape class-name
                       (class-patch-old-slot-specs cp)
                       (class-patch-old-superclass-names cp))
    (let ((instances (instances-to-reverse class-name)))
      ;; each touch lands in MIGRATE-INSTANCE-BACKWARD
      (dolist (entry instances)
        (handler-case (touch-instance (car entry))
          (migration-failure (e)
            (error 'rollback-failure :patch-set ps :cause e))))
      (remhash class-name *migration-registry*)
      (remhash class-name *tracked-classes*)
      (purge-class-tracking class-name)
      (let ((report (reversal-counts instances)))
        (audit :class-shape :class class-name :shape :original
                            :patch-set (patch-set-name ps)
                            :reversal-counts report)
        report))))

(defun promote-class-patch (ps cp)
  "Apply the final shape (dropped slots physically removed)."
  (let ((class-name (class-patch-class-name cp)))
    (setf (gethash class-name *migration-registry*) (cons cp :finalize))
    (apply-class-shape class-name
                       (class-patch-final-slots cp)
                       (class-patch-old-superclass-names cp))
    (eager-migrate-population class-name)
    (remhash class-name *migration-registry*)
    (remhash class-name *tracked-classes*)
    ;; drop the population; the journal stays until SEAL-PATCH as evidence
    (purge-class-tracking class-name :journal nil)
    (audit :class-shape :class class-name :shape :new
                        :patch-set (patch-set-name ps)
                        :dropped (class-patch-drop-slots cp))))

(defun purge-class-tracking (class-name &key (journal t) (population t))
  "Release tracking entries for CLASS-NAME."
  (sb-thread:with-mutex (*migration-lock*)
    (flet ((purge (table test)
             (let ((victims '()))
               (maphash (lambda (k v) (when (funcall test v) (push k victims)))
                        table)
               (dolist (k victims) (remhash k table))
               (length victims))))
      (list :journal (if journal
                         (purge *migration-journal*
                                (lambda (rec) (eq (getf rec :set) class-name)))
                         0)
            :population (if population
                            (purge *canary-population*
                                   (lambda (birth) (eq (car birth) class-name)))
                            0)))))

(defun seal-class-journal (ps)
  "Release journal entries for PS's classes; rollback is no longer offered."
  (let ((released 0))
    (sb-thread:with-mutex (*migration-lock*)
      (dolist (cp (patch-set-class-patches ps))
        (let ((victims '()))
          (maphash (lambda (inst record)
                     (when (eq (getf record :set) (class-patch-class-name cp))
                       (push inst victims)))
                   *migration-journal*)
          (dolist (inst victims)
            (remhash inst *migration-journal*)
            (incf released)))))
    (audit :journal-expired :patch-set (patch-set-name ps) :released released)
    released))

(defun migration-journal-count (&optional class-name)
  (sb-thread:with-mutex (*migration-lock*)
    (if class-name
        (let ((n 0))
          (maphash (lambda (inst record)
                     (declare (ignore inst))
                     (when (eq (getf record :set) class-name) (incf n)))
                   *migration-journal*)
          n)
        (hash-table-count *migration-journal*))))
