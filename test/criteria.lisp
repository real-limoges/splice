;;;; criteria.lisp: executes every success criterion and writes
;;;; artifacts/criteria-report.md with the measured numbers.
;;;;
;;;;   sbcl --non-interactive --eval '(require :asdf)' \
;;;;        --eval '(asdf:load-asd (truename "splice.asd"))' \
;;;;        --eval '(asdf:test-system "splice")'              ; full (~2-4 min)
;;;;
;;;; SPLICE_RUNS=20 in the environment gives a quicker run. TEST-OP signals
;;;; an error when any criterion fails, so a non-interactive run exits
;;;; nonzero. From a REPL, (splice-criteria:run-criteria :runs 20) returns
;;;; T when every criterion passes.

(defpackage :splice-criteria
  (:use :cl :splice :splice/harness)
  (:export #:run-criteria))
(in-package :splice-criteria)

(defvar *runs* 100 "Sessions per fault class; bound by RUN-CRITERIA.")
(defvar *failures* '())
(defvar *report* '())
(defvar *batches* '())
(defvar *big-log* nil "The log every section after the checkpoints shares.")

(defun root () (asdf:system-source-directory "splice"))
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
    (open-audit-log log)
    (reset-workload)
    (let ((ps (register-currency-patch-set)))
      (check "Checkpoint: pre-patch reconstruction"
             (verify-reconstruction log) nil)
      (install ps)
      (loop for i from 1 to 400
            do (with-task (:id i :swallow-errors t)
                 (process-order (make-instance 'order :id i :item-count 2
                                                      :unit-price 10.0))))
      (multiple-value-bind (ok mism state) (verify-reconstruction log)
        (check "Checkpoint: mid-canary reconstruction"
               (and ok (splice::canary-stage-p
                        (cdr (assoc (patch-set-name ps) (getf state :sets)))))
               (or mism (cdr (assoc (patch-set-name ps) (getf state :sets))))))
      (roll-back ps :operator "criteria")
      (wait-for-state ps '(:rolled-back :frozen))
      (check "Checkpoint: post-rollback reconstruction"
             (verify-reconstruction log) nil))
    (close-audit-log)))

;;; ---------------------------------------------------------------------
;;; Resume after a hard trigger (own log, like the checkpoints)

(defun check-resume-after-hard-trigger ()
  (format t "~%--- resume after a hard trigger on a lossy set~%")
  (let ((log (scratch-file "splice-criteria-resume.log")))
    (open-audit-log log)
    (reset-workload)
    (let ((corrupted (first (make-pre-existing-orders (make-rng 41) 1 :corrupted 1)))
          (ps (register-currency-patch-set :lossy t)))
      (install ps)
      ;; the corrupted order fails forward migration and arms the hard trigger;
      ;; a lossy set freezes for a human rather than rolling back
      (let ((*fault* (make-fault :kind :migration)))
        (with-task (:id 1 :swallow-errors t)
          (process-order corrupted)))
      (check "Resume: hard trigger freezes a lossy set for a human"
             (and (eq (patch-set-state ps) :frozen)
                  (eq (first (splice::patch-set-freeze-reason ps))
                      :lossy-reversal-needs-human))
             (list (patch-set-state ps) (splice::patch-set-freeze-reason ps)))
      (unfreeze ps :resume :operator "criteria")
      (with-task (:id 2 :swallow-errors t)
        (process-order (make-test-order)))
      (check "Resume: unfreeze :resume survives the next task"
             (splice::canary-stage-p (patch-set-state ps))
             (patch-set-state ps)))
    (reset-workload)
    (close-audit-log)))

;;; ---------------------------------------------------------------------
;;; C4 pre-work: the pristine workload signature, captured before any patch

(defun workload-signature (seed n)
  (reset-workload)
  (let ((rng (make-rng seed)))
    (loop for i from 1 to n
          collect (with-task (:id i)
                    (process-order
                     (make-instance 'order :id i
                                           :item-count (rng-range rng 1 9)
                                           :unit-price (/ (rng-range rng 100 9999) 100.0)
                                           :discount-code (case (mod (rng-next rng) 5)
                                                            (0 "TEN") (1 "VIP") (t nil))))))))

;;; ---------------------------------------------------------------------
;;; C1: the flagship scenario

(defun check-flagship ()
  (format t "~%--- C1: flagship, 3 functions + method + class, latent fault, 5/25/100 ramp~%")
  (let ((r (run-canary-session :fault :latent :seed 4242 :quiet t)))
    (check "Flagship: fault caught and rolled back"
           (eq (session-result-final-state r) :rolled-back)
           (session-result-final-state r))
    (let ((cmp (session-result-post-rollback-comparison r)))
      (check "Flagship: fdefinitions EQ to originals" (getf cmp :fdefinitions-eq) cmp)
      (check "Flagship: class shape restored" (getf cmp :class-shape-original) nil)
      (check "Flagship: every pre-existing instance restored to snapshot"
             (and cmp (= (getf cmp :snapshot-restored) (getf cmp :snapshot-total)))
             (format nil "~a/~a" (getf cmp :snapshot-restored)
                     (getf cmp :snapshot-total))))
    (let ((rev (first (session-result-reversal-report r))))
      ;; a report exists only when every instance reversed: the first
      ;; failure aborts the rollback and freezes the set
      (check "Flagship: reverse migration completed without failures"
             (and rev t)
             rev)
      (check "Flagship: journal-exact restorations occurred (instances were
migrated forward mid-canary, then restored)"
             (and rev (plusp (getf rev :journal-exact))) rev))
    (check "Flagship: zero mixed executions" (zerop (session-result-mixed-executions r))
           (session-result-mixed-executions r))))

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

(defstruct batch kind runs (caught 0) stages detect-ns detect-tasks)

(defun highest-canary-stage (r)
  (loop with best = 0
        for (state . nil) in (session-result-transitions r)
        when (splice::canary-stage-p state)
          do (setf best (max best (parse-integer (subseq (symbol-name state) 7))))
        finally (return best)))

(defun run-batch (kind &key threads)
  (let ((b (make-batch :kind kind :runs *runs*)))
    (dotimes (i *runs*)
      (let* ((r (run-canary-session :fault kind
                                    :seed (+ 10000 (* 131 i)
                                             (if kind (sxhash kind) 0))
                                    :threads (or threads 1) :quiet t))
             (state (session-result-final-state r)))
        (unless (zerop (session-result-mixed-executions r))
          (check (format nil "~a run ~a: mixed executions" kind i) nil
                 (session-result-mixed-executions r)))
        (when (eq state :rolled-back)
          (incf (batch-caught b))
          (push (session-result-detection-observations r) (batch-detect-ns b))
          (push (session-result-tasks-run r) (batch-detect-tasks b))
          (push (highest-canary-stage r) (batch-stages b)))))
    b))

(defun check-clean-batch ()
  "Spurious rollbacks of a clean patch measure the realized false-positive rate."
  (format t "clean batch...~%")
  (let ((b (run-batch nil)))
    (push (cons :clean b) *batches*)
    (let* ((k (batch-caught b))        ; every rollback of a clean patch is spurious
           (ci (wilson-ci k *runs*)))
      (check (format nil "C6: clean-patch spurious rollbacks ~a/~a, 95% CI [~,4f, ~,4f] contains alpha=0.01"
                     k *runs* (first ci) (second ci))
             (and (<= (first ci) 0.01) (<= 0.01 (second ci))
                  ;; and never via a nonsense route: a clean patch must mostly promote
                  (>= (- *runs* k) (* 0.9 *runs*)))
             nil))))

(defun check-fault-batches ()
  (dolist (kind (all-fault-kinds))
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
  (check "C3: zero mixed executions across the entire criteria run"
         (zerop (coherence-violations)) (coherence-violations)))

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
  (reset-workload)
  (let* ((bench-order (make-instance 'order :id 1 :item-count 3 :unit-price 19.99))
         (plain (ns-per-call `(lambda (n)
                                (let ((o ,bench-order))
                                  (dotimes (i n) (price-order o)))))))
    (let ((ps (register-currency-patch-set)))
      (install ps)
      ;; canary in flight: measure the shim on both routes, inside task scope
      (let* ((shim-old (let ((splice::*task-epochs* (list (list ps :old)))
                             (splice::*task-shim-routes* '()))
                         (ns-per-call `(lambda (n)
                                         (let ((o ,bench-order))
                                           (dotimes (i n) (price-order o)))))))
             (shim-new (let ((splice::*task-epochs* (list (list ps :new)))
                             (splice::*task-shim-routes* '()))
                         (ns-per-call `(lambda (n)
                                         (let ((o ,(make-instance 'order :id 2
                                                                         :item-count 3 :unit-price 19.99)))
                                           (dotimes (i n) (price-order o))))))))
        (promote ps :operator "criteria")
        (wait-for-state ps '(:promoted) :timeout 30)
        (let ((promoted (ns-per-call `(lambda (n)
                                        (let ((o ,(make-instance 'order :id 3
                                                                        :item-count 3 :unit-price 19.99)))
                                          (dotimes (i n) (price-order o)))))))
          (push (list :plain plain :shim-old shim-old :shim-new shim-new
                      :promoted promoted)
                *report*)
          (format t "plain old: ~,1fns  shim(:old): ~,1fns (~,2fx)  shim(:new): ~,1fns  promoted new: ~,1fns~%"
                  plain shim-old (/ shim-old plain) shim-new promoted)
          (check (format nil "C7: canary shim overhead ~,2fx < 2x plain call"
                         (/ shim-old plain))
                 (< (/ shim-old plain) 2.0) nil)
          (check "C7: post-promotion shim is structurally gone (fdefinition EQ new implementation)"
                 (eq (fdefinition 'price-order) #'splice/harness::new-price-order)
                 nil))))))

;;; ---------------------------------------------------------------------
;;; C5b: the whole criteria log: chain intact, reconstruction matches image

(defun check-audit-log ()
  "Caller has closed *BIG-LOG*."
  (format t "~%--- C5: audit chain + reconstruction over the full criteria log~%")
  (multiple-value-bind (ok entry-count break-seq) (verify-audit-chain *big-log*)
    (check (format nil "C5: hash chain intact over ~a entries" entry-count)
           ok break-seq))
  (multiple-value-bind (ok mismatches) (verify-reconstruction *big-log*)
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
    (multiple-value-bind (ok entry-count break-seq) (verify-audit-chain tampered)
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
        (*big-log* (scratch-file "splice-criteria.log")))
    (format t "~%=== splice criteria runner (~a runs per class) ===~%" *runs*)
    (check-reconstruction-checkpoints)
    (check-resume-after-hard-trigger)
    (open-audit-log *big-log*)
    (let ((pristine (workload-signature 777 300)))
      (check-flagship)
      (check-post-rollback-workload pristine))
    (format t "~%--- C2/C6: ~a-run batches per class~%" *runs*)
    (check-clean-batch)
    (check-fault-batches)
    (check-coherence)
    (check-overhead)
    (close-audit-log)
    (check-audit-log)
    (write-report)
    (format t "~%=== ~:[~a FAILURES~;all criteria passed~]; report: artifacts/criteria-report.md ===~%"
            (null *failures*) (length *failures*))
    (null *failures*)))
