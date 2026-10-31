;;;; chain.lisp

(in-package #:splice-orders/demo)

(defun chain-result (path)
  "Does the log at PATH chains, how many entries it holds, and the first
one to not trust"
  (multiple-value-bind (ok verified break-seq) (splice:verify-audit-chain path)
    (declare (ignore verified))
    (list :ok ok
          :entries (nth-value 1 (splice::map-audit-entries path nil :verify nil))
          :break-seq break-seq
          :first-bad-seq (and break-seq (1- break-seq)))))

(defun reconstruction-text (path)
  "One line saying whether state rebuilt from the log, at PATH, matches the live image"
  (multiple-value-bind (ok mismatches) (splice:verify-reconstruction path)
    (if ok
        "State rebuilt from the log matches the live image"
        (format nil "State rebuilt from the log differs in ~d place~:p" (length mismatches)))))

(defun tamper-copy (path &key (line 40))
  "Copy the log at PATH to temp directory with one digit changed on LINE.
Returns the copy's path - PATH is read only."
  (let ((copy (merge-pathnames "splice-demo-audit-tampered.log" (uiop:temporary-directory))))
    (with-open-file (in path)
      (with-open-file (out copy :direction :output :if-exists :supersede)
        (loop for text = (read-line in nil) while text
              for i from 0
              do (write-line (if (= i line) (substitute #\9 #\1 text :count 1) text) out))))
    copy))
