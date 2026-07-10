;;;; demo.lisp — the full splice cycle, narrated.
;;;;
;;;;   sbcl --non-interactive --load demo.lisp
;;;;
;;;; Act I:  a clean patch set (3 interdependent functions + a method + a
;;;;         class redefinition) rides the 5% -> 25% -> 100% ramp and is
;;;;         promoted.
;;;; Act II: the same patch set with a latent fault (fails only for VIP
;;;;         orders) is caught by the SPRT mid-ramp and fully reverted —
;;;;         including reverse migration of every instance that had already
;;;;         been migrated forward.
;;;; Act III: the audit log's hash chain is verified, the image state is
;;;;         reconstructed from the log alone, and tampering is detected.

(require :asdf)
(asdf:load-asd (merge-pathnames "splice.asd" *load-truename*))
(asdf:load-system "splice")

(defpackage :splice-demo (:use :cl :splice :splice/harness))
(in-package :splice-demo)

(defvar *log-path* (merge-pathnames "artifacts/demo-audit.log"
                                    (asdf:system-source-directory "splice")))

(defun headline (fmt &rest args)
  (format t "~%~a~%~a~%" (apply #'format nil fmt args)
          (make-string 72 :initial-element #\-)))

(open-audit-log *log-path*)
(format t "~%splice demo — audit log: ~a~%" *log-path*)

;;; --------------------------------------------------------------------
(headline "ACT I — clean patch: canary ramp 5% -> 25% -> 100% -> promoted")

(let ((r (run-canary-session :fault nil :seed 1001 :quiet t)))
  (format t "final state: ~a after ~a tasks~%"
          (session-result-final-state r) (session-result-tasks-run r))
  (format t "lifecycle:   ~{~a~^ -> ~}~%"
          (mapcar #'car (session-result-transitions r)))
  (format t "mixed executions (must be 0): ~a~%"
          (session-result-mixed-executions r))
  ;; the patched world is live: order math is exact minor units now
  (let ((o (make-instance 'order :id 9999 :item-count 3 :unit-price 19.99
                                 :discount-code "TEN")))
    (with-task (:id 999999)
      (process-order o)
      (format t "post-promotion order total: ~a cents (exact rational math)~%"
              (order-cents o)))))

;;; --------------------------------------------------------------------
(headline "ACT II — latent fault: caught mid-ramp, fully reverted")

(format t "fault: ~a~%~%" (fault-description :latent))
(let ((r (run-canary-session :fault :latent :seed 2002 :quiet t)))
  (format t "final state: ~a after ~a tasks~%"
          (session-result-final-state r) (session-result-tasks-run r))
  (format t "lifecycle:   ~{~a~^ -> ~}~%"
          (mapcar #'car (session-result-transitions r)))
  (format t "SPRT rejected after ~a canary observations~%"
          (session-result-detection-observations r))
  (format t "reverse migration: ~a~%" (session-result-reversal-report r))
  (format t "rollback exactness: ~a~%"
          (session-result-post-rollback-comparison r))
  (format t "mixed executions (must be 0): ~a~%"
          (session-result-mixed-executions r)))

;; show that old behavior is genuinely back
(let ((o (make-instance 'order :id 10000 :item-count 3 :unit-price 19.99
                               :discount-code "TEN")))
  (with-task (:id 1000000)
    (process-order o)
    (format t "post-rollback order total: $~,2f (float world restored; ~
class has no minor-units slot: ~a)~%"
            (order-float-total o)
            (not (slot-exists-p o 'splice/harness::amount-minor-units)))))

;;; --------------------------------------------------------------------
(headline "ACT III — the audit log is the source of truth")

(close-audit-log)

(multiple-value-bind (ok entry-count) (verify-audit-chain *log-path*)
  (format t "hash chain intact over ~a entries: ~a~%" entry-count ok))

(multiple-value-bind (ok mismatches state) (verify-reconstruction *log-path*)
  (declare (ignore state))
  (format t "state reconstructed from log matches live image: ~a~a~%"
          ok (if mismatches (format nil " (~a)" mismatches) "")))

;; tamper with a copy; the chain must break
(let ((tampered (merge-pathnames "splice-demo-audit-tampered.log"
                                 (uiop:temporary-directory))))
  (with-open-file (in *log-path*)
    (with-open-file (out tampered :direction :output :if-exists :supersede)
      (loop for line = (read-line in nil) while line
            for i from 0
            do (write-line (if (= i 40)
                               (substitute #\9 #\1 line :count 1)
                               line)
                           out))))
  (multiple-value-bind (ok entry-count break-seq) (verify-audit-chain tampered)
    (declare (ignore entry-count))
    (format t "tampered copy detected: intact=~a, chain breaks at seq ~a~%"
            ok break-seq)))

(format t "~%demo complete.~%")
