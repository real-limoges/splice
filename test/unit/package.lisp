;;;; package.lisp: the unit suite's package, root suite, and shared fixtures.
;;;;
;;;;   sbcl --non-interactive --eval '(require :asdf)' \
;;;;        --eval '(asdf:load-asd (truename "splice.asd"))' \
;;;;        --eval '(asdf:test-system "splice")'
;;;;
;;;; From a REPL, (fiveam:run! 'splice-tests:splice-tests) runs everything
;;;; and (fiveam:run! 'splice-tests::sprt) runs one file's suite. The tests reach
;;;; into internals with SPLICE:: because most of what is worth pinning down
;;;; is not exported.

(defpackage #:splice-tests
  (:use #:common-lisp #:fiveam #:splice)
  (:export #:splice-tests))
(in-package #:splice-tests)

(def-suite splice-tests :description "Unit tests for splice.")

;;; Audit logs

(defun temp-log-path ()
  (merge-pathnames (format nil "splice-unit-~a.log" (random most-positive-fixnum
                                                            (make-random-state t)))
                   (uiop:temporary-directory)))

(defmacro with-audit-log ((path) &body body)
  "Run BODY with a fresh audit log open at PATH, bound to a local
*AUDIT-LOG* so the global one is never touched. The file is deleted after."
  `(let ((,path (temp-log-path))
         (*audit-log* nil))
     (open-audit-log ,path)
     (unwind-protect (progn ,@body)
       (close-audit-log)
       (uiop:delete-file-if-exists ,path))))

(defun log-entries (path)
  "Every entry of the log at PATH, oldest first, chain not checked."
  (let ((entries '()))
    (splice::map-audit-entries path (lambda (entry) (push entry entries))
                               :verify nil)
    (nreverse entries)))

(defun log-lines (path)
  (uiop:read-file-lines path))

;;; Patch sets

(defmacro with-patch-set-names ((&rest names) &body body)
  "Run BODY, then drop the patch sets NAMES from the registry whatever
happened, so one test's sets never overlap with the next test's."
  `(unwind-protect (progn ,@body)
     (dolist (name ',names) (unregister-patch-set name))))

(defun contract (name check-fn)
  (splice::make-contract :name name :check-fn check-fn :source (list name)))

(defun fn-patch-set (set-name fn-name new-fn &key contracts)
  "An unregistered patch set replacing FN-NAME with NEW-FN."
  (splice::make-patch-set
   :name set-name
   :ramp '(50)
   :fn-patches (list (splice::make-fn-patch
                      :name fn-name
                      :new-fn new-fn
                      :new-source (list fn-name :new)
                      :new-source-hash (sha256-hex (string fn-name))))
   :contracts contracts))

(defun veto-of (thunk)
  "The PATCH-VETOED condition THUNK signals, or NIL when it returns."
  (handler-case (progn (funcall thunk) nil)
    (patch-vetoed (c) c)))
