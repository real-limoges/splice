;;;; conditions.lisp — the splice condition hierarchy (DESIGN.md §7.4).
;;;; All splice decisions travel as conditions so operators at a REPL can
;;;; interact with a live incident through standard restart machinery.

(in-package #:splice)

(define-condition splice-condition (condition)
  ((patch-set :initarg :patch-set :initform nil :reader splice-condition-patch-set)))

(define-condition patch-vetoed (splice-condition error)
  ((contract :initarg :contract :reader patch-vetoed-contract)
   (detail :initarg :detail :initform nil :reader patch-vetoed-detail))
  (:report (lambda (c stream)
             (format stream "Patch set ~a vetoed by ~a check: ~a"
                     (name-of (splice-condition-patch-set c))
                     (patch-vetoed-contract c)
                     (patch-vetoed-detail c)))))

;; Signaled (not raised as an error) each time new-routed code misbehaves;
;; the evaluator listens, the audit log records, and the task's own error
;; handling proceeds unless paired fallback intercepts.
(define-condition canary-anomaly (splice-condition)
  ((task :initarg :task :reader canary-anomaly-task)
   (cause :initarg :cause :reader canary-anomaly-cause))
  (:report (lambda (c stream)
             (format stream "Canary anomaly in task ~a: ~a"
                     (canary-anomaly-task c) (canary-anomaly-cause c)))))

(define-condition canary-verdict (splice-condition)
  ((stream-kind :initarg :stream-kind :reader canary-verdict-stream)
   (direction :initarg :direction :reader canary-verdict-direction) ; :accept / :reject
   (n :initarg :n :reader canary-verdict-n)
   (llr :initarg :llr :reader canary-verdict-llr))
  (:report (lambda (c stream)
             (format stream "SPRT ~a on ~a stream after n=~a (llr=~,3f) for ~a"
                     (canary-verdict-direction c) (canary-verdict-stream c)
                     (canary-verdict-n c) (canary-verdict-llr c)
                     (name-of (splice-condition-patch-set c))))))

(define-condition migration-failure (splice-condition error)
  ((instance :initarg :instance :reader migration-failure-instance)
   (direction :initarg :direction :reader migration-failure-direction)
   (cause :initarg :cause :reader migration-failure-cause))
  (:report (lambda (c stream)
             (format stream "Instance migration (~a) failed for ~a: ~a"
                     (migration-failure-direction c)
                     (migration-failure-instance c)
                     (migration-failure-cause c)))))

(define-condition rollback-initiated (splice-condition)
  ((trigger :initarg :trigger :reader rollback-initiated-trigger))
  (:report (lambda (c stream)
             (format stream "Rollback of ~a initiated: ~a"
                     (name-of (splice-condition-patch-set c))
                     (rollback-initiated-trigger c)))))

(define-condition rollback-failure (splice-condition error)
  ((cause :initarg :cause :reader rollback-failure-cause))
  (:report (lambda (c stream)
             (format stream "ROLLBACK FAILURE for ~a (set frozen): ~a"
                     (name-of (splice-condition-patch-set c))
                     (rollback-failure-cause c)))))

;; NAME-OF tolerates conditions built before the patch-set struct exists.
(defun name-of (patch-set-or-nil)
  (if patch-set-or-nil (patch-set-name patch-set-or-nil) "<none>"))
