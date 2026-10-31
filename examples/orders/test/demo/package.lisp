;;;; package.lisp

(defpackage #:splice-orders/demo-tests
  (:use #:common-lisp #:fiveam)
  (:local-nicknames (#:orders #:splice-orders) (#:d #:splice-orders/demo))
  (:export #:demo))

(in-package #:splice-orders/demo-tests)

(defparameter *fixture-names*
  '("act-1-paused-stage-0" "act-1-canary-25" "act-1-promoting-held"
    "act-1-promoted" "act-2-reject" "act-2-rolled-back")
  "The hand-written snapshots in test/demo/fixtures/, in act order.")

(def-suite demo :description "Demo tests that run without CLOG")

;;; Audit logs (every thread can see)

(defun fresh-log-path ()
  (merge-pathnames (format nil "splice-demo-~a.log"
                           (random most-positive-fixnum (make-random-state t)))
                   (uiop:temporary-directory)))

(defun call-with-demo-log (fn)
  "Call FN with path of a fresh audit log opened as global *AUDIT-LOG*.
Worker, straggler, and completion threads writ to it. Previous global log
comes back afterwards and the file is deleted"
  (let ((previous splice:*audit-log*)
        (path (fresh-log-path)))
    (splice:open-audit-log path)
    (unwind-protect (funcall fn path)
      (splice:close-audit-log)
      (setf splice:*audit-log* previous)
      (uiop:delete-file-if-exists path))))

(defmacro with-demo-log ((path) &body body)
  "Run BODY with PATH bound to the log CALL-WITH-DEMO-LOG opens up"
  `(call-with-demo-log (lambda (,path) ,@body)))

;;; Snapshot Fixtures

(defun load-fixture (name)
  "Snapshot in test/demo/fixtures/NAME.sexp"
  (with-open-file (in (asdf:system-relative-pathname
                       "splice-orders" (format nil "test/demo/fixtures/~a.sexp" name)))
    (d:read-snapshot in)))

(defun all-fixtures ()
  (mapcar #'load-fixture *fixture-names*))
