;;;; lifecycle.lisp — the patch-set state machine (DESIGN.md §3), the
;;;; canary evaluator (§6–7), and rollback/promotion completion. Rollback
;;;; and promotion complete in their own threads: the observation that
;;;; triggers them arrives inside a pinned task, and completion must wait
;;;; for exactly such tasks to drain.

(in-package #:splice)

(defvar *drain-timeout-seconds* 10
  "How long rollback/promotion waits for in-flight pinned tasks before
freezing (rollback) or noting unswitched work (promotion).")

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
  "New SPRT pair for a stage: error stream tracks the control arm's rate;
latency stream tests exceedance of the control p95, so p0=0.05 fixed."
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

;;; ---------------------------------------------------------------------
;;; INSTALL: contracts gate -> shims (dormant) -> atomic flip to first stage

(defun install (name-or-ps)
  "Take a REGISTERED patch set through contracts into its first canary
stage. Signals PATCH-VETOED if any contract fails; nothing is installed in
that case."
  (let ((ps (if (patch-set-p name-or-ps) name-or-ps
                (find-patch-set name-or-ps))))
    (assert (eq (patch-set-state ps) :registered) ()
            "~a is ~a, not :registered" (patch-set-name ps)
            (patch-set-state ps))
    ;; the union class shape must exist before contracts run: contracts
    ;; exercise the new functions, and new code reads the added slots.
    ;; Adding slots is behaviorally invisible to old code, and a veto
    ;; reverts the shape through the same reverse-migration machinery.
    (dolist (cp (patch-set-class-patches ps))
      (begin-class-canary ps cp))
    ;; contract veto gate: runs against the new functions directly; no shim
    ;; exists yet, so a veto means no live code path ever routed :new
    (handler-case (run-contracts ps :phase :pre-flight)
      (patch-vetoed (e)
        (dolist (cp (patch-set-class-patches ps))
          (rollback-class-patch ps cp))
        (error e)))
    (setf (patch-set-control-ring ps) (make-control-ring))
    ;; install shims dormant (they route 100% :old until the stage flip)
    (install-fn-shims ps)
    (install-method-shims ps)
    (sb-thread:with-mutex ((patch-set-lock ps))
      (setf (patch-set-state ps) :staged)
      (push (cons :staged (get-universal-time)) (patch-set-history ps))
      (audit :install :patch-set (patch-set-name ps)
             :shimmed-functions (mapcar #'fn-patch-name
                                        (patch-set-fn-patches ps))
             :shimmed-methods (mapcar #'method-patch-gf-name
                                      (patch-set-method-patches ps))
             :union-classes (mapcar #'class-patch-class-name
                                    (patch-set-class-patches ps)))
      ;; the commit: one state flip and the first stage is live
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

;;; ---------------------------------------------------------------------
;;; The evaluator: every task outcome flows through here (via WITH-TASK)

(defun report-outcome (ps route failure-p latency-ms &key condition)
  "Fold one task outcome into PS's evaluator. Control (:old) outcomes feed
the baseline; canary (:new) outcomes feed the SPRTs and may trigger a
transition. Returns the action taken, if any."
  (declare (ignorable condition))
  (sb-thread:with-mutex ((patch-set-lock ps))
    (let ((ring (patch-set-control-ring ps)))
      (unless (canary-stage-p (patch-set-state ps))
        (when (and ring (eq route :old))
          (control-record ring failure-p latency-ms))
        (return-from report-outcome nil))
      ;; hard triggers (A2) fire regardless of the observing task's route: a
      ;; migration failure can surface in an old-routed task's touch, and at
      ;; a 5% ramp the next canary observation could be a long time coming
      (when (patch-set-hard-trigger ps)
        (initiate-rollback-locked ps (cons :hard-trigger
                                           (patch-set-hard-trigger ps)))
        (return-from report-outcome :rolling-back))
      (cond
        ((eq route :old)
         (control-record ring failure-p latency-ms)
         ;; contemporaneous baseline: refresh H0 as the control arm evolves
         (let ((rate (control-failure-rate ring)))
           (when rate
             (unless (sprt-fixed-p0 (patch-set-sprt-error ps))
               (sprt-set-p0 (patch-set-sprt-error ps) (max rate 1d-3)))
             ;; confound check (H2): if the control arm itself is degraded,
             ;; a failing canary verdict would be superstition
             (when (> rate (patch-set-confound-threshold ps))
               (freeze-locked ps (list :confounded :control-rate rate))
               (return-from report-outcome :frozen))))
         nil)
        (t                              ; :new — a canary observation
         (evaluate-canary-observation ps failure-p latency-ms))))))

(defun evaluate-canary-observation (ps failure-p latency-ms)
  "Caller holds the lock; hard triggers were already checked upstream."
  (let* ((err-sprt (patch-set-sprt-error ps))
         (lat-sprt (patch-set-sprt-latency ps))
         (ring (patch-set-control-ring ps))
         (p95 (control-latency-quantile ring 0.95))
         (err-verdict (sprt-observe err-sprt failure-p))
         ;; latency exceedance with a noise margin: strictly above
         ;; max(1.5*p95, p95+0.5ms) counts (DESIGN.md A4)
         (lat-verdict (if p95
                          (sprt-observe lat-sprt
                                        (> latency-ms
                                           (max (* 1.5 p95) (+ p95 0.5))))
                          :continue)))
    ;; runtime contract sampling: structural breakage is not a rate question
    (when (and (plusp (patch-set-runtime-contract-interval ps))
               (zerop (mod (sprt-n err-sprt)
                           (patch-set-runtime-contract-interval ps))))
      (let ((failing (run-contracts ps :phase :runtime)))
        (when (and failing (not (eq failing t)))
          (initiate-rollback-locked ps (list :runtime-contract failing))
          (return-from evaluate-canary-observation :rolling-back))))
    (cond
      ;; either stream rejecting is a rollback
      ((or (eq err-verdict :reject) (eq lat-verdict :reject))
       (let ((stream (if (eq err-verdict :reject) :errors :latency))
             (sprt (if (eq err-verdict :reject) err-sprt lat-sprt)))
         (signal 'canary-verdict :patch-set ps :stream-kind stream
                                 :direction :reject
                                 :n (sprt-n sprt) :llr (sprt-llr sprt))
         (initiate-rollback-locked
          ps (list :sprt-reject (sprt-snapshot sprt stream))))
       :rolling-back)
      ;; stage advances only when BOTH streams accept health
      ((and (eq err-verdict :accept) (eq lat-verdict :accept))
       (signal 'canary-verdict :patch-set ps :stream-kind :both
                               :direction :accept
                               :n (sprt-n err-sprt) :llr (sprt-llr err-sprt))
       (if (< (1+ (patch-set-stage-index ps)) (length (patch-set-ramp ps)))
           (progn (enter-stage ps (1+ (patch-set-stage-index ps)))
                  :advanced)
           (progn (begin-promotion-locked ps) :promoting)))
      ;; stage budget exhausted between the boundaries: H1, never a silent
      ;; promote
      ((or (eq err-verdict :undecided) (eq lat-verdict :undecided))
       (freeze-locked ps (list :stage-timeout
                               :errors (sprt-snapshot err-sprt :errors)
                               :latency (sprt-snapshot lat-sprt :latency)))
       :frozen)
      (t :continue))))

;;; ---------------------------------------------------------------------
;;; Rollback (§4.3, §5.4)

(defun initiate-rollback-locked (ps trigger)
  "Caller holds the lock. Flips routing off instantly (new tasks pin :old
from here on) and hands completion to a drain thread."
  ;; H4: a lossy class reversal must not proceed automatically
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
  (signal 'rollback-initiated :patch-set ps :trigger trigger)
  (sb-thread:make-thread (lambda () (complete-rollback ps))
                         :name (format nil "splice-rollback-~a"
                                       (patch-set-name ps))))

(defun wait-for-drain (counter-fn timeout-seconds)
  (loop with deadline = (+ (get-internal-real-time)
                           (* timeout-seconds internal-time-units-per-second))
        while (plusp (funcall counter-fn))
        when (> (get-internal-real-time) deadline)
          do (return nil)
        do (sleep 0.005)
        finally (return t)))

(defun complete-rollback (ps)
  "Runs in its own thread: drain :new-pinned tasks, restore the old world,
reverse-migrate instances, verify, audit."
  (handler-case
      (progn
        (unless (wait-for-drain (lambda () (patch-set-pinned-new ps))
                                *drain-timeout-seconds*)
          (error 'rollback-failure :patch-set ps
                 :cause (format nil ":new-pinned tasks failed to drain in ~as"
                                *drain-timeout-seconds*)))
        (remove-fn-shims ps :world :old)
        (remove-method-shims ps :world :old)
        (let ((reversal-reports
                (mapcar (lambda (cp) (rollback-class-patch ps cp))
                        (patch-set-class-patches ps))))
          ;; verify restoration is exact before declaring success
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
          (sb-thread:with-mutex ((patch-set-lock ps))
            (transition ps :rolled-back))
          (audit :rollback-complete :patch-set (patch-set-name ps)
                 :restoration-verified t
                 :reversal reversal-reports)))
    (rollback-failure (e)
      ;; H3: the safety net itself failed — stop escalating, page a human
      (sb-thread:with-mutex ((patch-set-lock ps))
        (freeze-locked ps (list :rollback-failure (princ-to-string e))))
      (audit :rollback-failed :patch-set (patch-set-name ps)
             :cause (princ-to-string e)))))

;;; ---------------------------------------------------------------------
;;; Promotion (§4.4)

(defun begin-promotion-locked (ps)
  (transition ps :promoting)
  (sb-thread:make-thread (lambda () (complete-promotion ps))
                         :name (format nil "splice-promote-~a"
                                       (patch-set-name ps))))

(defun complete-promotion (ps)
  "Quiescence check, then the destructive install of the new world."
  (let ((drained (wait-for-drain (lambda () (patch-set-pinned-old ps))
                                 *drain-timeout-seconds*)))
    ;; H5: unswitched task-scoped work is only noted — old code stays valid
    ;; right up to the swap, and non-task threads adopt new code on their
    ;; next call through the swapped fdefinition
    (unless drained
      (audit :promotion-quiescence :patch-set (patch-set-name ps)
             :unswitched-old-pinned (patch-set-pinned-old ps)))
    (remove-fn-shims ps :world :new)
    (remove-method-shims ps :world :new)
    (dolist (cp (patch-set-class-patches ps))
      (promote-class-patch ps cp))
    (sb-thread:with-mutex ((patch-set-lock ps))
      (transition ps :promoted))
    (audit :promotion :patch-set (patch-set-name ps)
           :functions (mapcar (lambda (fp)
                                (list :name (fn-patch-name fp)
                                      :installed (fn-patch-new-source-hash fp)))
                              (patch-set-fn-patches ps))
           :journal-retained (migration-journal-count))))

;;; ---------------------------------------------------------------------
;;; Operator entry points and FROZEN handling

(defun freeze-locked (ps reason)
  (setf (patch-set-freeze-reason ps) reason
        (patch-set-current-fraction ps) 0)
  (transition ps :frozen :reason reason)
  (audit :freeze :patch-set (patch-set-name ps) :reason reason))

(defun resolve-patch-set (name-or-ps)
  (if (patch-set-p name-or-ps) name-or-ps (find-patch-set name-or-ps)))

(defun freeze (name reason &key (operator "unspecified"))
  (let ((ps (resolve-patch-set name)))
    (sb-thread:with-mutex ((patch-set-lock ps))
      (freeze-locked ps (list :operator operator :reason reason)))))

(defun unfreeze (name decision &key (operator "unspecified"))
  "Resolve a FROZEN set: DECISION is :resume, :rollback, or :promote.
Every resolution is audited with the operator's identity."
  (let ((ps (resolve-patch-set name)))
    (sb-thread:with-mutex ((patch-set-lock ps))
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
  "Operator-initiated rollback of a canarying set."
  (let ((ps (resolve-patch-set name)))
    (sb-thread:with-mutex ((patch-set-lock ps))
      (initiate-rollback-locked ps (list :operator operator)))))

(defun promote (name &key (operator "unspecified"))
  "Operator-forced promotion (bypasses remaining ramp stages)."
  (let ((ps (resolve-patch-set name)))
    (sb-thread:with-mutex ((patch-set-lock ps))
      (audit :force-promote :patch-set (patch-set-name ps) :operator operator)
      (begin-promotion-locked ps))))

(defun seal-patch (name &key (operator "unspecified"))
  "Release the migration journal; rollback of this set is no longer offered."
  (let ((ps (resolve-patch-set name)))
    (setf (patch-set-sealed ps) t)
    (audit :sealed :patch-set (patch-set-name ps) :operator operator)
    (seal-class-journal ps)))

(defun reverse-migration-report (name)
  "The reversal counts recorded by the last completed rollback of NAME."
  (let ((ps (resolve-patch-set name)))
    (and ps (patch-set-last-reversal-report ps))))

(defun unregister-patch-set (name)
  (remhash name *patch-sets*))

(defun wait-for-state (name-or-ps states &key (timeout 30))
  "Block until the set reaches one of STATES (rollback/promotion complete in
their own threads). Returns the state reached, or NIL on timeout."
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
;;; Audit-log reconstruction vs. the live image (criterion §10.5)

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
