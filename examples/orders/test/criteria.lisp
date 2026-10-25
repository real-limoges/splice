;;;; criteria.lisp: executes every success criterion and writes
;;;; artifacts/criteria-report.md (under examples/orders/) with the measured
;;;; numbers. From the repository root:
;;;;
;;;;   sbcl --non-interactive --eval '(require :asdf)' \
;;;;        --eval '(asdf:load-asd (truename "splice.asd"))' \
;;;;        --eval '(asdf:load-asd (truename "examples/orders/splice-orders.asd"))' \
;;;;        --eval '(asdf:test-system "splice-orders/criteria")'     ; full (~2-4 min)
;;;;
;;;; SPLICE_RUNS=20 in the environment gives a quicker run. TEST-OP signals
;;;; an error when any criterion fails, so a non-interactive run exits
;;;; nonzero. From a REPL, (splice-orders/criteria:run-criteria :runs 20)
;;;; returns T when every criterion passes.

(defpackage :splice-orders/criteria
  (:use :cl)
  (:local-nicknames (#:orders #:splice-orders))
  (:export #:run-criteria))
(in-package :splice-orders/criteria)

(defstruct batch kind runs (caught 0) final-states stages detect-ns detect-tasks)

(defvar *runs* 100 "Sessions per fault class; bound by RUN-CRITERIA.")
(defvar *failures* '())
(defvar *report* '())
(defvar *batches* '())
(defvar *big-log* nil "The log every section after the checkpoints shares.")
(defvar *mixed-executions* 0
  "Mixed executions summed over every canary session in the run. The session driver
resets its own counter at the start of each session.")

(defun root () (asdf:system-source-directory "splice-orders"))
(defun scratch-file (name) (merge-pathnames name (uiop:temporary-directory)))

(defun default-runs ()
  (let ((v (sb-ext:posix-getenv "SPLICE_RUNS")))
    (if v (parse-integer v) 100)))

(defun check (name ok detail)
  (format t "~:[FAIL~;ok  ~] ~a~@[: ~a~]~%" ok name detail)
  (unless ok (push (list name detail) *failures*))
  (push (list name ok detail) *report*)
  ok)

(defun median (numbers)
  (let ((s (sort (remove nil numbers) #'<)))
    (when s (nth (floor (length s) 2) s))))

(defun wilson-ci (k n &optional (z 1.96))
  "95% Wilson score interval for a binomial proportion."
  (let* ((p (/ k (float n)))
         (z2n (/ (* z z) n))
         (center (/ (+ p (/ z2n 2)) (1+ z2n)))
         (half (/ (* z (sqrt (+ (/ (* p (- 1 p)) n) (/ z2n n 4)))) (1+ z2n))))
    (list (max 0.0 (- center half)) (min 1.0 (+ center half)))))

;;; ---------------------------------------------------------------------
;;; C5a: reconstruction checkpoints (own log so the fold is single-session)

(defun check-reconstruction-checkpoints ()
  (format t "~%--- reconstruction checkpoints (pre-patch / mid-canary / post-rollback)~%")
  (let ((log (scratch-file "splice-criteria-checkpoint.log")))
    (splice:open-audit-log log)
    (orders:reset-workload)
    (let ((ps (orders:register-currency-patch-set)))
      (check "Checkpoint: pre-patch reconstruction"
             (splice:verify-reconstruction log) nil)
      (splice:install ps)
      (loop for i from 1 to 400
            do (splice:with-task (:id i :swallow-errors t)
                 (orders:process-order (make-instance 'orders:order :id i :item-count 2
                                                          :unit-price 10.0))))
      (multiple-value-bind (ok mism state) (splice:verify-reconstruction log)
        (check "Checkpoint: mid-canary reconstruction"
               (and ok (splice::canary-stage-p
                        (cdr (assoc (splice:patch-set-name ps) (getf state :sets)))))
               (or mism (cdr (assoc (splice:patch-set-name ps) (getf state :sets))))))
      (splice:roll-back ps :operator "criteria")
      (splice:wait-for-state ps '(:rolled-back :frozen))
      (check "Checkpoint: post-rollback reconstruction"
             (splice:verify-reconstruction log) nil))
    (splice:close-audit-log)))

;;; ---------------------------------------------------------------------
;;; Resume after a hard trigger (own log, like the checkpoints)

(defun check-resume-after-hard-trigger ()
  (format t "~%--- resume after a hard trigger on a lossy set~%")
  (let ((log (scratch-file "splice-criteria-resume.log")))
    (splice:open-audit-log log)
    (orders:reset-workload)
    (let ((corrupted (first (orders:make-pre-existing-orders (orders:make-rng 41) 1 :corrupted 1)))
          (ps (orders:register-currency-patch-set :lossy t)))
      (splice:install ps)
      ;; the corrupted order fails forward migration and arms the hard trigger;
      ;; a lossy set freezes for a human rather than rolling back
      (let ((orders:*fault* (orders:make-fault :kind :migration)))
        (splice:with-task (:id 1 :swallow-errors t)
          (orders:process-order corrupted)))
      (check "Resume: hard trigger freezes a lossy set for a human"
             (and (eq (splice:patch-set-state ps) :frozen)
                  (eq (first (splice::patch-set-freeze-reason ps))
                      :lossy-reversal-needs-human))
             (list (splice:patch-set-state ps) (splice::patch-set-freeze-reason ps)))
      (splice:unfreeze ps :resume :operator "criteria")
      (splice:with-task (:id 2 :swallow-errors t)
        (orders:process-order (orders:make-test-order)))
      (check "Resume: unfreeze :resume survives the next task"
             (splice::canary-stage-p (splice:patch-set-state ps))
             (splice:patch-set-state ps)))
    (orders:reset-workload)
    (splice:close-audit-log)))

;;; ---------------------------------------------------------------------
;;; Runtime contracts see old code on the old route (own log)

(defun route-probe (x)
  "Old side of the probe patch: names the code that ran. Call it through its
symbol: SBCL derives :OLD as the return type of a same-file DEFUN and would
type-check the shim's :NEW into an error."
  (declare (ignore x))
  :old)

(defun join-rollback-thread (ps)
  "Wait for PS's rollback thread, which audits after its last state change."
  (let ((name (format nil "splice-rollback-~a" (splice:patch-set-name ps))))
    (dolist (thread (sb-thread:list-all-threads))
      (when (equal (sb-thread:thread-name thread) name)
        (sb-thread:join-thread thread :default nil)))))

(defun check-runtime-contract-routing ()
  (format t "~%--- runtime contracts call old code through the shims~%")
  (let* ((log (scratch-file "splice-criteria-runtime-contract.log"))
         (seen '())
         (ps (splice::make-patch-set
              :name 'route-probe-patch
              :ramp '(50)
              :runtime-contract-interval 10
              :fn-patches
              (list (splice::make-fn-patch
                     :name 'route-probe
                     :new-fn (lambda (x) (declare (ignore x)) :new)
                     :new-source '(route-probe-new)
                     :new-source-hash (splice:sha256-hex "route-probe-new")))
              :contracts
              (list (splice::make-contract
                     :name 'old-call-reaches-old-code
                     ;; a plain call is the contract's "old" side; it must
                     ;; reach old code at pre-flight and at runtime alike
                     :check-fn (lambda ()
                                 (let ((ran (funcall 'route-probe 1)))
                                   (push ran seen)
                                   (eq ran :old)))
                     :source '(old-call-reaches-old-code))))))
    (splice:open-audit-log log)
    (splice:unregister-patch-set 'route-probe-patch)
    (splice:register-patch-set ps)
    (splice:install ps)
    ;; about half of these route :new; every tenth canary observation runs
    ;; the contracts from inside that task's cleanup
    (loop for i from 1 to 60
          do (splice:with-task (:id i) (funcall 'route-probe i)))
    (check "Runtime contract: a plain call inside a canary task reaches old code"
           (and (> (length seen) 1)
                (every (lambda (ran) (eq ran :old)) seen))
           (reverse seen))
    (when (splice::canary-stage-p (splice:patch-set-state ps))
      (splice:roll-back ps :operator "criteria"))
    (splice:wait-for-state ps '(:rolled-back :frozen))
    (join-rollback-thread ps)
    (splice:unregister-patch-set 'route-probe-patch)
    (splice:close-audit-log)))

;;; ---------------------------------------------------------------------
;;; C4 pre-work: the pristine workload signature, captured before any patch

(defun workload-signature (seed n)
  "Totals from N seeded orders run against whatever code and class shape are
live now. It does not reset the workload, so the post-rollback capture sees
what the rollback left behind."
  (let ((rng (orders:make-rng seed)))
    (loop for i from 1 to n
          collect (splice:with-task (:id i)
                    (orders:process-order
                     (make-instance 'orders:order :id i
                                             :item-count (orders:rng-range rng 1 9)
                                             :unit-price (/ (orders:rng-range rng 100 9999) 100.0)
                                             :discount-code (case (mod (orders:rng-next rng) 5)
                                                              (0 "TEN") (1 "VIP") (t nil))))))))

;;; ---------------------------------------------------------------------
;;; C1: the flagship scenario

(defun run-session (&rest args)
  "RUN-CANARY-SESSION, adding the session's mixed executions to the run total."
  (let ((r (apply #'orders:run-canary-session args)))
    (incf *mixed-executions* (orders:coherence-violations))
    r))

(defun check-flagship ()
  (format t "~%--- C1: flagship, 3 functions + method + class, latent fault, 5/25/100 ramp~%")
  (let ((r (run-session :fault :latent :seed 4242 :quiet t)))
    (check "Flagship: fault caught and rolled back"
           (eq (orders:session-result-final-state r) :rolled-back)
           (orders:session-result-final-state r))
    (let ((cmp (orders:session-result-post-rollback-comparison r)))
      (check "Flagship: fdefinitions EQ to originals" (getf cmp :fdefinitions-eq) cmp)
      (check "Flagship: class shape restored" (getf cmp :class-shape-original) nil)
      (check "Flagship: every pre-existing instance restored to snapshot"
             (and cmp (= (getf cmp :snapshot-restored) (getf cmp :snapshot-total)))
             (format nil "~a/~a" (getf cmp :snapshot-restored)
                     (getf cmp :snapshot-total))))
    (let ((rev (first (orders:session-result-reversal-report r))))
      ;; a report exists only when every instance reversed: the first
      ;; failure aborts the rollback and freezes the set
      (check "Flagship: reverse migration completed without failures"
             (and rev t)
             rev)
      (check "Flagship: journal-exact restorations occurred (instances were
migrated forward mid-canary, then restored)"
             (and rev (plusp (getf rev :journal-exact))) rev))
    (check "Flagship: zero mixed executions" (zerop (orders:coherence-violations))
           (orders:coherence-violations))))

;;; C4: post-rollback workload identical to never-patched control

(defun check-post-rollback-workload (pristine)
  (let ((post (workload-signature 777 300)))
    (check "C4: post-rollback workload identical to never-patched control"
           (equal pristine post)
           (format nil "~a/~a totals equal"
                   (count t (mapcar #'equal pristine post))
                   (length post)))))

;;; ---------------------------------------------------------------------
;;; C2/C6: fault matrix + statistical honesty over *runs* sessions/class

(defun batch-count (state b)
  (count state (batch-final-states b)))

(defun highest-canary-stage (r)
  (loop with best = 0
        for (state . nil) in (orders:session-result-transitions r)
        when (splice::canary-stage-p state)
          do (setf best (max best (parse-integer (subseq (symbol-name state) 7))))
        finally (return best)))

(defun run-batch (kind &key threads)
  (let ((b (make-batch :kind kind :runs *runs*)))
    (dotimes (i *runs*)
      (let* ((r (run-session :fault kind
                                    :seed (+ 10000 (* 131 i)
                                             (if kind (sxhash kind) 0))
                                    :threads (or threads 1) :quiet t))
             (state (orders:session-result-final-state r)))
        (push state (batch-final-states b))
        (unless (zerop (orders:coherence-violations))
          (check (format nil "~a run ~a: mixed executions" kind i) nil
                 (orders:coherence-violations)))
        (when (eq state :rolled-back)
          (incf (batch-caught b))
          (push (orders:session-result-detection-observations r) (batch-detect-ns b))
          (push (orders:session-result-tasks-run r) (batch-detect-tasks b))
          (push (highest-canary-stage r) (batch-stages b)))))
    b))

(defun check-clean-batch ()
  "Spurious rollbacks of a clean patch measure the realized false-positive rate."
  (format t "clean batch...~%")
  (let ((b (run-batch nil)))
    (push (cons :clean b) *batches*)
    (let* ((k (batch-caught b))        ; every rollback of a clean patch is spurious
           (ci (wilson-ci k *runs*))
           (promoted (batch-count :promoted b)))
      (check (format nil "C6: clean-patch spurious rollbacks ~a/~a, 95% CI [~,4f, ~,4f] contains alpha=0.01"
                     k *runs* (first ci) (second ci))
             (and (<= (first ci) 0.01) (<= 0.01 (second ci))
                  ;; and never via a nonsense route: a clean patch must mostly
                  ;; promote, and a frozen, vetoed or undecided session is not
                  ;; a promotion
                  (>= promoted (* 0.9 *runs*)))
             (format nil "promoted ~a, rolled back ~a, frozen ~a, vetoed ~a, other ~a"
                     promoted k (batch-count :frozen b) (batch-count :vetoed b)
                     (- *runs* promoted k (batch-count :frozen b)
                        (batch-count :vetoed b)))))))

(defun check-fault-batches ()
  (dolist (kind (orders:all-fault-kinds))
    (format t "~a batch...~%" kind)
    (let ((b (run-batch kind :threads (when (eq kind :load-correlated) 8))))
      (push (cons kind b) *batches*)
      (check (format nil "C2/C6: ~a detection rate ~a/~a >= 0.95 (1-beta)"
                     kind (batch-caught b) *runs*)
             (>= (batch-caught b) (* 0.95 *runs*)) nil)
      (when (eq kind :load-correlated)
        (let ((at-5 (count 5 (batch-stages b)))
              (at-25+ (count-if (lambda (s) (>= s 25)) (batch-stages b))))
          (check (format nil "C2b: load-correlated slips past 5% (caught at 5%: ~a, at 25%+: ~a)"
                         at-5 at-25+)
                 (and (>= at-25+ (* 0.9 (batch-caught b))) (< at-5 (* 0.1 *runs*)))
                 nil))))))

;;; ---------------------------------------------------------------------
;;; C3: coherence, verified offline from the audit log

(defun check-coherence ()
  (format t "~%--- C3: offline coherence check from sampled task records~%")
  (let ((checked 0) (bad 0))
    (with-open-file (in *big-log*)
      (let ((*read-eval* nil) (*package* (find-package :splice)))
        (loop for line = (read-line in nil) while line
              for entry = (ignore-errors (read-from-string line))
              when (and entry (eq (getf entry :event) :task-sample))
                do (dolist (pin (getf entry :pins))
                     (let ((state (getf pin :state)))
                       ;; draws only govern canary stages; :promoting pins :new,
                       ;; draining states pin :old, by policy not by draw
                       (when (splice::canary-stage-p state)
                         (incf checked)
                         (let* ((draw (splice::routing-draw
                                       (getf entry :task)
                                       (getf pin :patch-set)))
                                (expect (if (< draw (getf pin :fraction))
                                            :new :old)))
                           (unless (eq expect (getf pin :route))
                             (incf bad)))))))))
    (check (format nil "C3: ~a sampled routes match the deterministic draw (~a bad)"
                   checked bad)
           (and (plusp checked) (zerop bad)) nil))
  (check "C3: zero mixed executions across every canary session"
         (zerop *mixed-executions*) *mixed-executions*))

;;; ---------------------------------------------------------------------
;;; C7: overhead

(defun ns-per-call (fn-form &key (iterations 500000))
  (let ((f (compile nil fn-form)))
    (sb-ext:gc :full t)
    (let ((start (get-internal-real-time)))
      (funcall f iterations)
      (/ (* (- (get-internal-real-time) start) 1.0e9)
         internal-time-units-per-second iterations))))

(defun check-overhead ()
  (format t "~%--- C7: shim overhead~%")
  (orders:reset-workload)
  (let* ((bench-order (make-instance 'orders:order :id 1 :item-count 3 :unit-price 19.99))
         (plain (ns-per-call `(lambda (n)
                                (let ((o ,bench-order))
                                  (dotimes (i n) (orders:price-order o)))))))
    (let ((ps (orders:register-currency-patch-set)))
      (splice:install ps)
      ;; canary in flight: measure the shim on both routes, inside task scope
      (let* ((shim-old (let ((splice::*task-epochs* (list (list ps :old)))
                             (splice::*task-shim-routes* '()))
                         (ns-per-call `(lambda (n)
                                         (let ((o ,bench-order))
                                           (dotimes (i n) (orders:price-order o)))))))
             (shim-new (let ((splice::*task-epochs* (list (list ps :new)))
                             (splice::*task-shim-routes* '()))
                         (ns-per-call `(lambda (n)
                                         (let ((o ,(make-instance 'orders:order :id 2
                                                                           :item-count 3 :unit-price 19.99)))
                                           (dotimes (i n) (orders:price-order o))))))))
        (splice:promote ps :operator "criteria")
        (splice:wait-for-state ps '(:promoted) :timeout 30)
        (let ((promoted (ns-per-call `(lambda (n)
                                        (let ((o ,(make-instance 'orders:order :id 3
                                                                          :item-count 3 :unit-price 19.99)))
                                          (dotimes (i n) (orders:price-order o)))))))
          (push (list :plain plain :shim-old shim-old :shim-new shim-new
                      :promoted promoted)
                *report*)
          (format t "plain old: ~,1fns  shim(:old): ~,1fns (~,2fx)  shim(:new): ~,1fns  promoted new: ~,1fns~%"
                  plain shim-old (/ shim-old plain) shim-new promoted)
          (check (format nil "C7: canary shim overhead ~,2fx < 2x plain call"
                         (/ shim-old plain))
                 (< (/ shim-old plain) 2.0) nil)
          (check "C7: post-promotion shim is structurally gone (fdefinition EQ new implementation)"
                 (eq (fdefinition 'orders:price-order) #'orders::new-price-order)
                 nil))))))

;;; ---------------------------------------------------------------------
;;; C5b: the whole criteria log: chain intact, reconstruction matches image

(defun check-audit-log ()
  "Caller has closed *BIG-LOG*."
  (format t "~%--- C5: audit chain + reconstruction over the full criteria log~%")
  (multiple-value-bind (ok entry-count break-seq) (splice:verify-audit-chain *big-log*)
    (check (format nil "C5: hash chain intact over ~a entries" entry-count)
           ok break-seq))
  (multiple-value-bind (ok mismatches) (splice:verify-reconstruction *big-log*)
    (check "C5: final image state reconstructed from log matches live image"
           ok mismatches))
  ;; tamper detection
  (let ((tampered (scratch-file "splice-criteria-tampered.log")))
    (with-open-file (in *big-log*)
      (with-open-file (out tampered :direction :output :if-exists :supersede)
        (loop for line = (read-line in nil) while line
              for i from 0
              do (write-line (if (= i 100) (substitute #\7 #\2 line :count 1) line)
                             out))))
    (multiple-value-bind (ok entry-count break-seq) (splice:verify-audit-chain tampered)
      (declare (ignore entry-count))
      (check "C5: tampering detected by chain verification"
             (and (not ok) break-seq) break-seq))))

;;; ---------------------------------------------------------------------
;;; report

(defun write-report ()
  (with-open-file (out (merge-pathnames "artifacts/criteria-report.md" (root))
                       :direction :output :if-exists :supersede)
    (format out "# Criteria Report~%~%")
    (format out "Generated by `test/criteria.lisp` (~a runs per class) on ~a, ~a.~%~%"
            *runs* (lisp-implementation-type) (lisp-implementation-version))
    (format out "## Verdicts~%~%")
    (format out "| Criterion | Result |~%|---|---|~%")
    (dolist (entry (reverse *report*))
      (when (and (listp entry) (= 3 (length entry)) (stringp (first entry)))
        (format out "| ~a | ~:[**FAIL**~;Pass~] |~%"
                (substitute #\space #\newline (first entry)) (second entry))))
    (format out "~%## Fault-Class Detection (n=~a Runs Each)~%~%" *runs*)
    (format out "| Fault | Caught | Median SPRT n | Median Tasks to Decision | Stages Caught At |~%|---|---|---|---|---|~%")
    (dolist (kb (reverse *batches*))
      (destructuring-bind (kind . b) kb
        (format out "| ~a | ~a/~a | ~a | ~a | ~a |~%"
                (or kind :clean) (batch-caught b) (batch-runs b)
                (or (median (batch-detect-ns b)) "N/A")
                (or (median (batch-detect-tasks b)) "N/A")
                (let ((s (remove-duplicates (batch-stages b))))
                  (if s (format nil "~{~a%~^, ~}" (sort s #'<)) "N/A")))))
    (let ((oh (find-if (lambda (e) (and (listp e) (eq (first e) :plain))) *report*)))
      (when oh
        (format out "~%## Overhead (C7)~%~%")
        (format out "| Path | ns/call |~%|---|---|~%")
        (format out "| Plain (pre-patch) | ~,1f |~%| Shim, :old route | ~,1f (~,2fx) |~%| Shim, :new route | ~,1f |~%| Promoted (shim gone) | ~,1f |~%"
                (getf oh :plain) (getf oh :shim-old)
                (/ (getf oh :shim-old) (getf oh :plain))
                (getf oh :shim-new) (getf oh :promoted))))
    (format out "~%## Outcome~%~%~:[**~a criteria FAILED**~;All criteria passed.~]~%"
            (null *failures*) (length *failures*))))

;;; ---------------------------------------------------------------------
;;; Entry point

(defun run-criteria (&key (runs (default-runs)))
  "Run every criterion in order, write artifacts/criteria-report.md, and
return T when all pass. The order matters: the checkpoints run on their own
log first, the pristine signature is captured before the flagship patch,
and the full-log checks run last."
  (let ((*runs* runs)
        (*failures* '())
        (*report* '())
        (*batches* '())
        (*mixed-executions* 0)
        (*big-log* (scratch-file "splice-criteria.log")))
    (format t "~%=== splice criteria runner (~a runs per class) ===~%" *runs*)
    (check-reconstruction-checkpoints)
    (check-resume-after-hard-trigger)
    (check-runtime-contract-routing)
    (splice:open-audit-log *big-log*)
    (orders:reset-workload)
    (let ((pristine (workload-signature 777 300)))
      (check-flagship)
      (check-post-rollback-workload pristine))
    (format t "~%--- C2/C6: ~a-run batches per class~%" *runs*)
    (check-clean-batch)
    (check-fault-batches)
    (check-coherence)
    (check-overhead)
    (splice:close-audit-log)
    (check-audit-log)
    (write-report)
    (format t "~%=== ~:[~a FAILURES~;all criteria passed~]; report: artifacts/criteria-report.md ===~%"
            (null *failures*) (length *failures*))
    (null *failures*)))
