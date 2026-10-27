;;;; lifecycle.lisp: patch-set state machine, canary evaluator, and
;;;; rollback/promotion. Those two complete in their own threads, since the
;;;; trigger arrives inside a pinned task that must drain first.

(in-package #:splice)

(defvar *drain-timeout-seconds* 10
  "How long rollback and promotion wait for pinned tasks to drain. Either one
freezes the set when the wait runs out.")

(defun transition (ps to &rest fields)
  "Move PS to state TO, record history, and audit. Caller holds the lock."
  (let ((from (patch-set-state ps)))
    (setf (patch-set-state ps) to)
    (push (cons to (get-universal-time)) (patch-set-history ps))
    (apply #'audit :canary-transition
           :patch-set (patch-set-name ps) :from from :to to fields)))

(defun patch-set-lifecycle-history (ps)
  (reverse (patch-set-history ps)))

(defun fresh-stage-sprts (ps)
  "New SPRT pair for a stage. Errors track the control arm's rate; latency
tests exceedance of the control p95, so its p0 is fixed at 0.05."
  (setf (patch-set-sprt-error ps)
        (make-sprt :alpha (patch-set-alpha ps) :beta (patch-set-beta ps)
                   :p0 (or (and (patch-set-control-ring ps)
                                (control-failure-rate (patch-set-control-ring ps)))
                           0.001d0)
                   :min-n (patch-set-sprt-min-n ps)
                   :max-n (patch-set-sprt-max-n ps))
        (patch-set-sprt-latency ps)
        (make-sprt :alpha (patch-set-alpha ps) :beta (patch-set-beta ps)
                   :p0 0.05d0 :p1 0.15d0 :fixed-p0 t
                   :min-n (patch-set-sprt-min-n ps)
                   :max-n (patch-set-sprt-max-n ps))))

;;; INSTALL: contracts gate -> shims (dormant) -> atomic flip to first stage

(defun install (name-or-ps)
  "Take a REGISTERED patch set through contracts into its first canary
stage. Signals PATCH-VETOED, installing nothing, if any contract fails."
  (let ((ps (if (patch-set-p name-or-ps) name-or-ps
                (find-patch-set name-or-ps))))
    (assert (eq (patch-set-state ps) :registered) ()
            "~a is ~a, not :registered" (patch-set-name ps)
            (patch-set-state ps))
    ;; contracts run new code, which reads the added slots, so the union
    ;; shape comes first. Old code cannot see it, and a veto reverts it.
    (dolist (cp (patch-set-class-patches ps))
      (begin-class-canary ps cp))
    ;; no shim exists yet, so a veto means nothing live ever routed :new
    (handler-case (run-contracts ps :phase :pre-flight)
      (patch-vetoed (e)
        (dolist (cp (patch-set-class-patches ps))
          (rollback-class-patch ps cp))
        (error e)))
    (setf (patch-set-control-ring ps) (make-control-ring))
    ;; shims start dormant, routing :old until the stage flip
    (install-fn-shims ps)
    (install-method-shims ps)
    (with-set-lock (ps)
      (setf (patch-set-state ps) :staged)
      (push (cons :staged (get-universal-time)) (patch-set-history ps))
      (audit :install :patch-set (patch-set-name ps)
                      :shimmed-functions (mapcar #'fn-patch-name
                                                 (patch-set-fn-patches ps))
                      :shimmed-methods (mapcar #'method-patch-gf-name
                                               (patch-set-method-patches ps))
                      :union-classes (mapcar #'class-patch-class-name
                                             (patch-set-class-patches ps)))
      ;; one state flip makes the first stage live
      (enter-stage ps 0))
    ps))

(defun enter-stage (ps index)
  "Enter ramp stage INDEX. Caller holds the lock."
  (let ((fraction (nth index (patch-set-ramp ps))))
    (setf (patch-set-stage-index ps) index
          (patch-set-current-fraction ps) fraction)
    (fresh-stage-sprts ps)
    (transition ps (stage-keyword fraction)
                :fraction fraction
                :sprt (sprt-snapshot (patch-set-sprt-error ps) :errors))))

;;; EVALUATOR: every task outcome flows through here (via WITH-TASK)

(defun report-outcome (ps route failure-p latency-ms &key condition)
  "Fold one task outcome into PS's evaluator. Control (:old) outcomes feed
the baseline; canary (:new) outcomes feed the SPRTs and may trigger a
transition. Returns the action taken: :continue when nothing changed, else
:advanced, :promoting, :rolling-back or :frozen."
  (declare (ignorable condition))
  (with-set-lock (ps)
    (report-outcome-locked ps route failure-p latency-ms)))

(defun report-outcome-locked (ps route failure-p latency-ms)
  "The locked stuff"
  (let ((ring (patch-set-control-ring ps)))
    (unless (canary-stage-p (patch-set-state ps))
      (when (and ring (eq route :old))
        (control-record ring failure-p latency-ms))
      (return-from report-outcome-locked :continue))
    ;; hard triggers fire on any route: a migration failure can surface in
    ;; an old-routed task, and at 5% the next canary task may be far off
    (let ((trigger (take-hard-trigger ps)))
      (when trigger
        (return-from report-outcome-locked
          (initiate-rollback-locked ps (cons :hard-trigger trigger)))))
    (cond
      ((eq route :old)
       (control-record ring failure-p latency-ms)
       ;; refresh H0 from the live control arm
       (let ((rate (control-failure-rate ring)))
         (when rate
           (unless (sprt-fixed-p0 (patch-set-sprt-error ps))
             (sprt-set-p0 (patch-set-sprt-error ps) (max rate 1d-3)))
           ;; a degraded control arm makes any canary verdict meaningless
           (when (> rate (patch-set-confound-threshold ps))
             (freeze-locked ps (list :confounded :control-rate rate))
             (return-from report-outcome-locked :frozen))))
       :continue)
      (t                              ; :new, a canary observation
       (evaluate-canary-observation ps failure-p latency-ms)))))

(defun evaluate-canary-observation (ps failure-p latency-ms)
  "Caller holds the lock; hard triggers were already checked upstream."
  (let* ((err-sprt (patch-set-sprt-error ps))
         (lat-sprt (patch-set-sprt-latency ps))
         (ring (patch-set-control-ring ps))
         (p95 (control-latency-quantile ring 0.95))
         (err-verdict (sprt-observe err-sprt failure-p))
         ;; slow means above max(1.5*p95, p95+0.5ms)
         (lat-verdict (if p95
                          (sprt-observe lat-sprt
                                        (> latency-ms
                                           (max (* 1.5 p95) (+ p95 0.5))))
                          :continue)))
    ;; sample runtime contracts: structural breakage is not a rate question
    (when (and (plusp (patch-set-runtime-contract-interval ps))
               (zerop (mod (sprt-n err-sprt)
                           (patch-set-runtime-contract-interval ps))))
      (let ((failing (run-contracts ps :phase :runtime)))
        (when (and failing (not (eq failing t)))
          (return-from evaluate-canary-observation
            (initiate-rollback-locked ps (list :runtime-contract failing))))))
    (cond
      ;; either stream rejecting is a rollback
      ((or (eq err-verdict :reject) (eq lat-verdict :reject))
       (let ((stream (if (eq err-verdict :reject) :errors :latency))
             (sprt (if (eq err-verdict :reject) err-sprt lat-sprt)))
         (defer (lambda () (signal 'canary-verdict :patch-set ps :stream-kind stream
                                                   :direction :reject
                                                   :n (sprt-n sprt) :llr (sprt-llr sprt))))
         (initiate-rollback-locked
          ps (list :sprt-reject (sprt-snapshot sprt stream)))))
      ;; stage advances only when both streams accept health
      ((and (eq err-verdict :accept) (eq lat-verdict :accept))
       (defer (lambda () (signal 'canary-verdict :patch-set ps :stream-kind :both
                                                 :direction :accept
                                                 :n (sprt-n err-sprt) :llr (sprt-llr err-sprt))))
       (if (< (1+ (patch-set-stage-index ps)) (length (patch-set-ramp ps)))
           (progn (enter-stage ps (1+ (patch-set-stage-index ps)))
                  :advanced)
           (progn (begin-promotion-locked ps) :promoting)))
      ;; stage budget spent without a verdict: freeze, never promote silently
      ((or (eq err-verdict :undecided) (eq lat-verdict :undecided))
       (freeze-locked ps (list :stage-timeout
                               :errors (sprt-snapshot err-sprt :errors)
                               :latency (sprt-snapshot lat-sprt :latency)))
       :frozen)
      (t :continue))))

;;; ---------------------------------------------------------------------
;;; Rollback

(defun initiate-rollback-locked (ps trigger)
  "Caller holds the lock. Routes new tasks to :old at once and hands
completion to a drain thread. Returns :rolling-back, or :frozen when a lossy
class reversal needs an operator."
  ;; a lossy class reversal must not proceed automatically
  (when (and (some #'class-patch-lossy-p (patch-set-class-patches ps))
             (not (eq (car (if (consp trigger) trigger (list trigger)))
                      :operator)))
    (freeze-locked ps (list :lossy-reversal-needs-human :trigger trigger))
    (return-from initiate-rollback-locked :frozen))
  (setf (patch-set-current-fraction ps) 0)
  (transition ps :rolling-back :trigger trigger)
  (audit :rollback-decision :patch-set (patch-set-name ps) :trigger trigger
                            :pinned-new (patch-set-pinned-new ps)
                            :pinned-old (patch-set-pinned-old ps))
  (defer (lambda () (signal 'rollback-initiated :patch-set ps :trigger trigger)))
  (defer (lambda () (sb-thread:make-thread (lambda () (complete-rollback ps))
                                           :name (format nil "splice-rollback-~a"
                                                         (patch-set-name ps)))))
  :rolling-back)

(defun wait-for-drain (counter-fn timeout-seconds)
  (loop with deadline = (+ (get-internal-real-time)
                           (* timeout-seconds internal-time-units-per-second))
        while (plusp (funcall counter-fn))
        when (> (get-internal-real-time) deadline)
          do (return nil)
        do (sleep 0.005)
        finally (return t)))

(defun claim-transition (ps from to)
  "Move PS from FROM to TO, unless something else moved it first. Returns
:claimed or :abandoned."
  (with-set-lock (ps)
    (if (eq (patch-set-state ps) from)
        (progn (transition ps to) :claimed)
        (abandon-locked ps from to))))

(defun freeze-from (ps from reason)
  "Freeze PS if it is still in FROM. Returns :frozen or :abandoned."
  (with-set-lock (ps)
    (if (eq (patch-set-state ps) from)
        (progn (freeze-locked ps reason) :frozen)
        (abandon-locked ps from :frozen))))

(defun abandon-locked (ps expected wanted)
  "Audit a completion thread giving way to a state change it did not make."
  (audit :transition-abandoned :patch-set (patch-set-name ps)
                               :expected expected
                               :found (patch-set-state ps)
                               :wanted wanted)
  :abandoned)

(defun complete-rollback (ps)
  "Drain :new-pinned tasks, then claim the set and restore the old world. A
set that left :rolling-back during the drain (an operator freeze, say) is
left alone. Returns :rolled-back, :frozen or :abandoned."
  (cond ((not (wait-for-drain (lambda () (patch-set-pinned-new ps))
                              *drain-timeout-seconds*))
         (fail-rollback ps :rolling-back
                        (make-condition 'rollback-failure
                                        :patch-set ps
                                        :cause (format nil ":new-pinned tasks failed to drain in ~as"
                                                       *drain-timeout-seconds*))))
        ((eq :claimed (claim-transition ps :rolling-back :restoring))
         (restore-old-world ps))
        (t :abandoned)))

(defun fail-rollback (ps from failure)
  "The safety net itself failed: freeze PS for a human, unless it already
left FROM. Returns :frozen or :abandoned."
  (let ((outcome (freeze-from ps from
                              (list :rollback-failure (princ-to-string failure)))))
    (when (eq outcome :frozen)
      (audit :rollback-failed :patch-set (patch-set-name ps)
                              :cause (princ-to-string failure)))
    outcome))

(defun restore-old-world (ps)
  "Swap the old functions and classes back in, reverse-migrate, verify. Runs
in :restoring, which an operator cannot move the set out of."
  (handler-case
      (progn
        (remove-fn-shims ps :world :old)
        (remove-method-shims ps :world :old)
        (let ((reversal-reports
                (mapcar (lambda (cp) (rollback-class-patch ps cp))
                        (patch-set-class-patches ps))))
          ;; verify restoration before declaring success
          (let ((unrestored
                  (remove-if (lambda (fp)
                               (eq (fdefinition (fn-patch-name fp))
                                   (fn-patch-old-fn fp)))
                             (patch-set-fn-patches ps))))
            (when unrestored
              (error 'rollback-failure :patch-set ps
                                       :cause (format nil "fdefinitions not restored: ~a"
                                                      (mapcar #'fn-patch-name unrestored)))))
          (setf (patch-set-last-reversal-report ps) reversal-reports)
          (with-set-lock (ps)
            (transition ps :rolled-back))
          (audit :rollback-complete :patch-set (patch-set-name ps)
                                    :restoration-verified t
                                    :reversal reversal-reports)
          :rolled-back))
    (rollback-failure (e)
      (fail-rollback ps :restoring e))))

;;; ---------------------------------------------------------------------
;;; Promotion

(defun begin-promotion-locked (ps)
  (transition ps :promoting)
  (defer (lambda () (sb-thread:make-thread (lambda () (complete-promotion ps))
                                           :name (format nil "splice-promote-~a"
                                                         (patch-set-name ps))))))

(defun complete-promotion (ps)
  "Drain :old-pinned tasks, then claim the set and swap in the new world. Drain that times
out freezes th set, so no :old task ever runs new code. Sets that leave :promoting
during the drain are left alone. Returns :promoted, :frozen, or :abandoned"
  (cond ((not (wait-for-drain (lambda () (patch-set-pinned-old ps))
                              *drain-timeout-seconds*))
         (freeze-from ps :promoting
                      (list :promotion-drain-timeout
                            :old-pinned (patch-set-pinned-old ps)
                            :timeout-seconds *drain-timeout-seconds*)))
        ((eq :claimed (claim-transition ps :promoting :swapping))
         (swap-in-new-world ps))
        (t :abandoned)))

(defun swap-in-new-world (ps)
  "The destructive install of the new world. Runs in :swapping, which an
operator cannot move the set out of."
  (remove-fn-shims ps :world :new)
  (remove-method-shims ps :world :new)
  (dolist (cp (patch-set-class-patches ps))
    (promote-class-patch ps cp))
  (with-set-lock (ps)
    (transition ps :promoted))
  (audit :promotion :patch-set (patch-set-name ps)
                    :functions (mapcar (lambda (fp)
                                         (list :name (fn-patch-name fp)
                                               :installed (fn-patch-new-source-hash fp)))
                                       (patch-set-fn-patches ps))
                    :journal-retained (migration-journal-count))
  :promoted)

;;; ---------------------------------------------------------------------
;;; Operator entry points and FROZEN handling

(defun freeze-locked (ps reason)
  (setf (patch-set-freeze-reason ps) reason
        (patch-set-current-fraction ps) 0)
  (transition ps :frozen :reason reason)
  (audit :freeze :patch-set (patch-set-name ps) :reason reason))

(defun resolve-patch-set (name-or-ps)
  (if (patch-set-p name-or-ps) name-or-ps (find-patch-set name-or-ps)))

(defun refuse-locked (ps requested)
  "Signal ILLEGAL-TRANSITION once the lock is released, so its handlers run
outside it. Returns :refused, which the caller never sees."
  (let ((from (patch-set-state ps)))
    (defer (lambda ()
             (error 'illegal-transition :patch-set ps :from from
                                        :requested requested))))
  :refused)

(defun freeze (name reason &key (operator "unspecified"))
  "Stop a set where it is for an operator. Refused once a completion thread
has claimed the set (:restoring, :swapping) or the set has finished."
  (let ((ps (resolve-patch-set name)))
    (with-set-lock (ps)
      (freeze-operator-locked ps (list :operator operator :reason reason)))))

(defun freeze-operator-locked (ps reason)
  (let ((state (patch-set-state ps)))
    (cond ((eq state :frozen) :no-change)
          ((or (canary-stage-p state)
               (member state '(:staged :rolling-back :promoting)))
           (freeze-locked ps reason)
           :frozen)
          (t (refuse-locked ps :freeze)))))

(defun unfreeze (name decision &key (operator "unspecified"))
  "Resolve a FROZEN set: DECISION is :resume, :rollback, or :promote.
Every resolution is audited with the operator's identity."
  (let ((ps (resolve-patch-set name)))
    (with-set-lock (ps)
      (assert (eq (patch-set-state ps) :frozen))
      (audit :unfreeze :patch-set (patch-set-name ps)
                       :operator operator :decision decision
                       :was-frozen-for (patch-set-freeze-reason ps))
      (setf (patch-set-freeze-reason ps) nil)
      (ecase decision
        (:resume (enter-stage ps (max 0 (patch-set-stage-index ps))))
        (:rollback (initiate-rollback-locked ps (list :operator operator)))
        (:promote (begin-promotion-locked ps))))))

(defun roll-back (name &key (operator "unspecified"))
  "Operator-initiated rollback of a canarying, staged or frozen set. Quiet
when the set is already rolling back or rolled back; refused otherwise, and
always once sealed."
  (let ((ps (resolve-patch-set name)))
    (with-set-lock (ps)
      (roll-back-locked ps operator))))

(defun roll-back-locked (ps operator)
  (let ((state (patch-set-state ps)))
    (cond ((or (rolling-back-p state) (eq state :rolled-back)) :no-change)
          ((patch-set-sealed ps) (refuse-locked ps :roll-back))
          ((or (canary-stage-p state) (member state '(:staged :frozen)))
           (initiate-rollback-locked ps (list :operator operator)))
          (t (refuse-locked ps :roll-back)))))

(defun promote (name &key (operator "unspecified"))
  "Operator-forced promotion of a canarying or frozen set (bypasses remaining
ramp stages). Quiet when the set is already promoting or promoted; refused
otherwise."
  (let ((ps (resolve-patch-set name)))
    (with-set-lock (ps)
      (promote-locked ps operator))))

(defun promote-locked (ps operator)
  (let ((state (patch-set-state ps)))
    (cond ((or (promoting-p state) (eq state :promoted)) :no-change)
          ((or (canary-stage-p state) (eq state :frozen))
           (audit :force-promote :patch-set (patch-set-name ps) :operator operator)
           (begin-promotion-locked ps)
           :promoting)
          (t (refuse-locked ps :promote)))))

(defun seal-patch (name &key (operator "unspecified"))
  "Release a promoted set's migration journal; rollback of this set is no
longer offered."
  (let ((ps (resolve-patch-set name)))
    (with-set-lock (ps)
      (seal-locked ps operator))))

(defun seal-locked (ps operator)
  (cond ((patch-set-sealed ps) :no-change)
        ((eq :promoted (patch-set-state ps))
         (setf (patch-set-sealed ps) t)
         (audit :sealed :patch-set (patch-set-name ps) :operator operator)
         ;; outside the set lock, so the migration lock never nests in it
         (defer (lambda () (seal-class-journal ps)))
         :sealed)
        (t (refuse-locked ps :seal))))

(defun reverse-migration-report (name)
  "The reversal counts recorded by the last completed rollback of NAME."
  (let ((ps (resolve-patch-set name)))
    (and ps (patch-set-last-reversal-report ps))))

(defun unregister-patch-set (name)
  (remhash name *patch-sets*))

(defun wait-for-state (name-or-ps states &key (timeout 30))
  "Block until the set reaches one of STATES. Returns it, or NIL on timeout."
  (let ((ps (if (patch-set-p name-or-ps) name-or-ps
                (find-patch-set name-or-ps)))
        (deadline (+ (get-internal-real-time)
                     (* timeout internal-time-units-per-second))))
    (loop
      (let ((state (patch-set-state ps)))
        (when (member state states) (return state))
        (when (> (get-internal-real-time) deadline) (return nil))
        (sleep 0.01)))))

;;; ---------------------------------------------------------------------
;;; Audit-log reconstruction vs. the live image

(defun expected-function-object (name impl)
  "The function object the log says should be installed at NAME."
  (let ((owner nil) (fp nil))
    (maphash (lambda (sname ps)
               (declare (ignore sname))
               (let ((hit (find name (patch-set-fn-patches ps)
                                :key #'fn-patch-name)))
                 (when hit (setf owner ps fp hit))))
             *patch-sets*)
    (when fp
      (values (cond ((eq impl :original) (fn-patch-old-fn fp))
                    ((eq impl :shimmed) (fn-patch-shim fp))
                    ((and (consp impl) (eq (car impl) :new))
                     (fn-patch-new-fn fp)))
              owner))))

(defun verify-reconstruction (path)
  "Reconstruct expected image state from the audit log at PATH and compare
against the live image. Returns (values ok mismatches state)."
  (let* ((state (reconstruct-state path))
         (mismatches '()))
    (loop for (name . impl) in (getf state :functions)
          do (multiple-value-bind (expected owner)
                 (expected-function-object name impl)
               (declare (ignore owner))
               (cond ((null expected)
                      (push (list name :no-registry-object impl) mismatches))
                     ((not (eq (fdefinition name) expected))
                      (push (list name :fdefinition-mismatch impl)
                            mismatches)))))
    (loop for (cname . shape) in (getf state :classes)
          for class = (find-class cname nil)
          do (let* ((cp (block find-cp
                          (maphash (lambda (k ps)
                                     (declare (ignore k))
                                     (let ((hit (find cname
                                                      (patch-set-class-patches ps)
                                                      :key #'class-patch-class-name)))
                                       (when hit (return-from find-cp hit))))
                                   *patch-sets*)
                          nil))
                    (expected-names
                      (when cp
                        (mapcar (lambda (s) (getf s :name))
                                (ecase shape
                                  (:original (class-patch-old-slot-specs cp))
                                  (:union (class-patch-union-slots cp))
                                  (:new (class-patch-final-slots cp))))))
                    (live-names
                      (when class
                        (sb-mop:finalize-inheritance class)
                        (mapcar #'sb-mop:slot-definition-name
                                (sb-mop:class-direct-slots class)))))
               (unless (and expected-names
                            (null (set-exclusive-or expected-names live-names)))
                 (push (list cname :class-shape-mismatch shape
                                   :expected expected-names :live live-names)
                       mismatches))))
    (values (null mismatches) mismatches state)))
