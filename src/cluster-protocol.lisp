;;;; cluster-protocol.lisp: cluster protocol with no transport
;;;;
;;;; One conductor and N members exchange lists headed by a keyword. PREPARE
;;;; ships a patch set as source text plus a hash; each member compiles it
;;;; and runs the static checks, and any veto aborts the cluster. COMMIT
;;;; starts each member's local canary. Members report SPRT health; the
;;;; conductor sums their LLRs and broadcasts ROLLBACK on a member's local
;;;; rollback or a pooled reject.
;;;;
;;;; Nothing here opens a socket or reads a stream. A transport hands what
;;;; arrives to CONDUCTOR-RECEIVE or MEMBER-RECEIVE, and gives each side an
;;;; adapter: an object with methods on SEND-TO-MEMBER (the conductor's) or
;;;; on SEND-TO-CONDUCTOR, MEMBER-COMMITTED and FINAL-REPORT-FIELDS (a
;;;; member's), which the protocol calls back through. Every value in a
;;;; message is a keyword, a string, an integer or a list of those, so any
;;;; wire that carries those carries the protocol unchanged.
;;;;
;;;; Limitations:
;;;; - contracts run at COMMIT (inside INSTALL), not PREPARE.
;;;; - members ramp independently; the conductor gates only rollback (T3).
;;;; - no partition handling and no timeouts; a transport reports a dead
;;;;   link as (:gone).

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

;;; Build patch set from a spec (member's side o PREPARE)

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

;;; Conductor's view of the cluster as a value.

(defstruct (member-view (:copier nil))
  (id nil         :read-only t)
  (stage :added   :read-only t)
  (state :unknown :read-only t)
  (llr-milli 0    :read-only t)
  (n 0            :read-only t)
  (terminal nil   :read-only t)
  (final nil      :read-only t))

(defun member-view-with (view &key (stage (member-view-stage view))
                                (state (member-view-state view))
                                (llr-milli (member-view-llr-milli view))
                                (n (member-view-n view))
                                (terminal (member-view-terminal view))
                                (final (member-view-final view)))
  "Copy of VIEW with the named fields replaced"
  (make-member-view :id (member-view-id view)
                    :stage stage
                    :state state
                    :llr-milli llr-milli
                    :n n
                    :terminal terminal
                    :final final))



(defstruct (cluster (:copier nil))
  (phase :idle :read-only t)
  (members '() :read-only t)
  (spec nil :read-only t)
  (upper-milli 0 :read-only t)
  (rollback-sent nil :read-only t))

(defun cluster-with (cluster &key (phase (cluster-phase cluster))
                               (members (cluster-members cluster))
                               (spec (cluster-spec cluster))
                               (rollback-sent (cluster-rollback-sent cluster)))
  "Copy of CLUSTER with the named fields replaced"
  (make-cluster :phase phase
                :members members
                :spec spec
                :upper-milli (cluster-upper-milli cluster)
                :rollback-sent rollback-sent))

(defun empty-cluster (alpha beta)
  "Cluster wiht no members, whose pooled SPRT rejects at Wald's upper
for ALPHA and BETA (as each member's SPRT does)"
  (make-cluster :upper-milli (round (* 1000 (log (/ (- 1d0 beta) alpha))))))

;;; Reading and Rebuilding the Value

(defun find-view (cluster id)
  (find id (cluster-members cluster) :key #'member-view-id :test #'equal))


(defun with-view (cluster view)
  "CLUSTER with the member view that has VIEW's ids replaced by VIEW"
  (cluster-with cluster
                :members (mapcar (lambda (old)
                                   (if (equal (member-view-id old)
                                              (member-view-id view))
                                       view
                                       old))
                                 (cluster-members cluster))))

(defun member-ids (cluster)
  (mapcar #'member-view-id (cluster-members cluster)))

(defun broadcast (cluster message)
  "Send effect to every member in the order they were added"
  (mapcar (lambda (id) (list :send id message)) (member-ids cluster)))

(defun all-members-p (cluster predicate)
  (every predicate (cluster-members cluster)))

(defun cluster-finals (cluster)
  "Each Member's final report, headed :MEMBER in order"
  (mapcar (lambda (view)
            (list* :member (member-view-id view) (member-view-final view)))
          (cluster-members cluster)))

(defun pooled-llr-milli (cluster)
  "Members' latest LLRs summed. Independent add"
  (reduce #'+ (cluster-members cluster) :key #'member-view-llr-milli))

(defun rollback-trigger (cluster)
  "Why the cluster should roll back right now or NIL"
  (cond ((some (lambda (view)
                 (or (eq :rolled-back (member-view-terminal view))
                     (member (member-view-state view) '(:rolling-back :restoring))))
               (cluster-members cluster))
         :member-local-reject)
        ((>= (pooled-llr-milli cluster) (cluster-upper-milli cluster))
         :pooled-sprt-reject)
        (t nil)))

;;; Step - Transition for one conductor event (add a member, ship or receive a message)
;;; Every function from here through progress returns the same
;;; (values next-cluster effects) where effects are
;;; (:send member-id message) or (:audit event . field)

(defun step-add-member (cluster id)
  "CLUSTER with member ID added. Members join only before patch ships"
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
      (:committed
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
         (values (with-view cluster (member-view-with view :state state
                                                           :terminal state))
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
  (if (member (cluster-phase cluster) '(:preparing :committing))
      (values (cluster-with cluster :phase :aborted)
              (cons (list :audit :cluster-abort :member id :reason reason)
                    (broadcast cluster '(:bye))))
      (values cluster '())))

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

;;; Progress: What the cluster calls for and dispatched on the phase.
;;; Returrns: (values next-cluster effects)

(defun progress-phase (cluster)
  "Whatever the folded CLUSTER calls for"
  (case (cluster-phase cluster)
    (:preparing
     (if (all-members-p cluster (lambda (view) (eq :prepared (member-view-stage view))))
         (values (cluster-with cluster :phase :committing)
                 (broadcast cluster '(:commit)))
         (values cluster '())))
    (:committing
     (if (all-members-p cluster (lambda (view) (eq :committed (member-view-stage view))))
         (multiple-value-bind (running effects)
             (progress-running (cluster-with cluster :phase :running))
           (values running
                   (cons (list :audit :cluster-commit :members (member-ids cluster))
                         effects)))
         (values cluster '())))
    (:running
     (progress-running cluster))
    (:reporting
     (if (all-members-p cluster #'member-view-final)
         (values (cluster-with cluster :phase :done)
                 (cons (list :audit :cluster-final :reports (cluster-finals cluster))
                       (broadcast cluster '(:bye))))
         (values cluster '())))
    (t
     (values cluster '()))))

(defun progress-running (cluster)
  "Roll back (if called for) then report back when every member is terminal"
  (multiple-value-bind (checked rollback) (progress-rollback cluster)
    (if (all-members-p checked #'member-view-terminal)
        (values (cluster-with checked :phase :reporting)
                (append rollback (broadcast checked '(:report))))
        (values checked rollback))))

(defun progress-rollback (cluster)
  "Broadcast :ROLLBACK on a member's local rollback or when the pooled
LLR reaches the upper bound"
  (let ((trigger (rollback-trigger cluster)))
    (if (and trigger (not (cluster-rollback-sent cluster)))
        (values (cluster-with cluster :rollback-sent t)
                (cons (list :audit :cluster-rollback
                            :pooled-llr (/ (pooled-llr-milli cluster) 1000.0)
                            :upper (/ (cluster-upper-milli cluster) 1000.0)
                            :trigger trigger)
                      (broadcast cluster '(:rollback))))
        (values cluster '()))))

;;; Conductor's Adapter - Each one defines a method for its own type

(defgeneric send-to-member (adapter member-id message)
  (:documentation "Deliver MESSAGE to the member ADAPTER knows as MEMBER-ID.
Conductor calls it while holding its lock, so method cannot call back
into that conductor on the same thread"))

;;; Conductor: Cluster value behidn a lock and the effects it calls for

(defstruct (conductor (:constructor %make-conductor (adapter cluster)))
  (adapter nil :read-only t)
  (lock (sb-thread:make-mutex :name "splice conductor") :read-only t)
  ;; mutable
  cluster)                              ; the current cluster value, replaced whole

(defun make-conductor (&key adapter (alpha 0.01d0) (beta 0.05d0))
  "A conductor with no members. Every message goes through SEND-TO-MEMBER
on ADAPTER. It goes in teh order it decided them and holds its lock"
  (%make-conductor adapter (empty-cluster alpha beta)))

(defun run-conductor-effect (conductor effect)
  (ecase (first effect)
    (:send (destructuring-bind (id message) (rest effect)
             (send-to-member (conductor-adapter conductor) id message)))
    (:audit (apply #'splice:audit (rest effect)))))

(defun conductor-advance (conductor step)
  "Apply STEP. Step is a function of the cluster and it returns
(values next effects) under a lock. So it stores NEXT, runs the effects,
and returns NEXT's phase"
  (sb-thread:with-mutex ((conductor-lock conductor))
    (multiple-value-bind (next effects) (funcall step (conductor-cluster conductor))
      (setf (conductor-cluster conductor) next)
      (dolist (effect effects)
        (run-conductor-effect conductor effect))
      (cluster-phase next))))

(defun conductor-add-member (conductor member-id)
  "Add MEMBER-ID before patch ships. Return the phase, :IDLE"
  (conductor-advance conductor
                     (lambda (cluster) (step-add-member cluster member-id))))

(defun conductor-ship (conductor patch-set-forms)
  "Start PREPARE. Send every member PATCH-SET-FORMS, a spec plist with hashes
computed here. Return the phase, :PREPARING"
  (let ((spec (spec-with-hash patch-set-forms)))
    (conductor-advance conductor (lambda (cluster) (step-ship cluster spec)))))

(defun conductor-receive (conductor member-id message)
  "Hand CONDUCTOR one MESSAGE from MEMBER-ID as the transport received it.
Safe to call from several threads! Returns the phase after it"
  (conductor-advance conductor
                     (lambda (cluster) (step-receive cluster member-id message))))

(defun conductor-phase (conductor)
  (cluster-phase (conductor-cluster conductor)))

(defun conductor-finals (conductor)
  "Each member's final report (:MEMBER id :STATE state ...) in the order added"
  (cluster-finals (conductor-cluster conductor)))

;;; Member's adapter - Each one defines methods for its own type.
;;; Only SEND-TO-CONDUCTOR has no default.

(defgeneric send-to-conductor (adapter message)
  (:documentation "Deliver MESSAGE from member to conductor. A member
can call it from several threads at once, so the method must be safe to
call like that"))

(defgeneric member-committed (adapter cluster-member patch-set)
  (:documentation "Called on the thread that delivered :COMMIT once the
PATCH-SET is installed on CLUSTER-MEMBER. Adapter with a workload starts it
and the workload calls MEMBER-SEND-HEALTH. Value is ignore"))

(defmethod member-committed (adapter cluster-member patch-set)
  "Adapter with no workload of its own"
  (declare (ignore adapter cluster-member patch-set))
  :no-workload)

(defgeneric final-report-fields (adapter patch-set)
  (:documentation "Extra fields for member's :FINAL report as a plist of wire values.
Called once PATCH-SET is terminal. Adapter that closes its audit log here closes it after
the set's last transition"))

(defmethod final-report-fields (adapter patch-set)
  "No extra fields"
  (declare (ignore adapter patch-set))
  '())

;;; Members. Called CLUSTER-MEMBER because MEMBER is a COMMON-LISP symbol.
;;; The accessors are MEMBER-, like the rest of the member functions due to a
;;; little trickery.

(defstruct (cluster-member (:conc-name member-)
                           (:constructor %make-cluster-member (adapter)))
  (adapter nil :read-only t)
  ;; mutable
  (patch-set nil))                      ; set exactly once by :prepare

(defun make-member (&key adapter)
  "Member with no patch set (yet) calling back into ADAPTER through
SEND-TO-CONDUCTOR, MEMBER-COMMITTED, and FINAL-REPORT-FIELDS"
  (%make-cluster-member adapter))

(defun member-reply (cluster-member message)
  (send-to-conductor (member-adapter cluster-member) message))

(defun member-receive (cluster-member message)
  "Act on MESSAGE from conductor and replying through SEND-TO-CONDUCTOR. Call
it from one thread in arrival order. :REPORT may block for up to 30 seconds.
Returns MESSAGE's kind"
  (ecase (first message)
    (:prepare (member-prepare cluster-member (getf (rest message) :spec)))
    (:commit (member-commit cluster-member))
    (:rollback (member-rollback cluster-member))
    (:report (member-report cluster-member))
    (:bye nil))
  (first message))

(defun member-prepare (cluster-member spec)
  (handler-case
      (let ((ps (build-patch-set spec)))
        (setf (member-patch-set cluster-member) ps)
        (member-reply cluster-member
                      (list :prepared :name (symbol-name (splice:patch-set-name ps)))))
    (error (e)
      (member-reply cluster-member (list :veto :reason (princ-to-string e))))))

(defun member-commit (cluster-member)
  (let ((ps (member-patch-set cluster-member)))
    (handler-case
        (progn
          (splice:install ps)
          (member-reply cluster-member '(:committed))
          (start-terminal-watch cluster-member ps)
          (member-committed (member-adapter cluster-member) cluster-member ps))
      (splice:patch-vetoed (e)
        (member-reply cluster-member (list :veto :reason (princ-to-string e)))))))

(defun start-terminal-watch (cluster-member ps)
  "Thread that tells conductor once PS reaches a terminal state or what state it
is in after 2 minutes"
  (sb-thread:make-thread
   (lambda ()
     (splice:wait-for-state ps '(:promoted :rolled-back :frozen) :timeout 120)
     (member-reply cluster-member (list :terminal :state (splice:patch-set-state ps))))
   :name (format nil "splice-member-watch-~a" (splice:patch-set-name ps))))

(defun member-rollback (cluster-member)
  "Roll set back if it is still 'canary-ing', otherwise answer :CONTINUE since3
the conductor gates only rollback"
  (let ((ps (member-patch-set cluster-member)))
    (member-reply cluster-member
                  (list :rollback-result
                        :outcome
                        (if (null ps)
                            :continue
                            (splice::with-set-lock (ps)
                              (if (splice::canary-stage-p (splice:patch-set-state ps))
                                  (splice::initiate-rollback-locked
                                   ps '(:conductor-command))
                                  :continue)))))))

(defun member-report (cluster-member)
  "Wait for a terminal state and send :FINAL with the adapter's extra fields"
  (let ((ps (member-patch-set cluster-member)))
    (splice:wait-for-state ps '(:promoted :rolled-back :frozen :vetoed) :timeout 30)
    (let ((extra (final-report-fields (member-adapter cluster-member) ps)))
      (member-reply cluster-member
                    (list* :final
                           :state (splice:patch-set-state ps)
                           :reversal (splice::patch-set-last-reversal-report ps)
                           extra)))))

(defun health-message (ps)
  "PS's error SPRT as a :HEALTH message, with the LLR in thousandths"
  (let ((sprt (splice::patch-set-sprt-error ps)))
    (list :health
          :state (splice:patch-set-state ps)
          :fraction (splice::patch-set-current-fraction ps)
          :n (splice:sprt-n sprt)
          :llr-milli (round (* 1000 (splice:sprt-llr sprt))))))

(defun member-send-health (cluster-member)
  "Send the conductor the member's current SPRT health. The host's workload
can call it as much as it wants"
  (member-reply cluster-member (health-message (member-patch-set cluster-member))))
