;;;; audit.lisp: log entries, the hash chain, and the reconstruction fold.

(in-package #:splice-tests)

(def-suite audit :in splice-tests)
(in-suite audit)

(defstruct opaque-thing x)

;;; ---------------------------------------------------------------------
;;; Making values loggable

(test flatten-newlines
  (is (string= "a b c" (splice::flatten-newlines (format nil "a~%b~cc" #\Return)))))

(test sanitize-keeps-readable-atoms
  (dolist (value (list 1 2.5 :kw 'sym "plain" nil t))
    (is (equal value (splice::sanitize-for-log value)))))

(test sanitize-flattens-strings-inside-conses
  (is (equal '(:a ("x y") . "z w")
             (splice::sanitize-for-log
              (list* :a (list (format nil "x~%y")) (format nil "z~%w"))))))

(test sanitize-describes-conditions
  (is (equal '(:condition "SIMPLE-ERROR" "boom here")
             (splice::sanitize-for-log
              (make-condition 'simple-error :format-control "boom~%here")))))

(test sanitize-hashes-unreadable-objects
  (let ((logged (splice::sanitize-for-log (make-opaque-thing :x 1))))
    (is (eq :opaque (first logged)))
    (is (= 16 (length (second logged))))
    (is (equal logged (splice::sanitize-for-log (make-opaque-thing :x 1)))))
  (is (eq :opaque (first (splice::sanitize-for-log #'car)))))

(test timestamp-is-iso-8601-utc-with-milliseconds
  (let ((ts (splice::timestamp-string)))
    (is (= 24 (length ts)))
    (is (char= #\T (char ts 10)))
    (is (char= #\. (char ts 19)))
    (is (char= #\Z (char ts 23)))
    (is (every #'digit-char-p (remove-if (lambda (c) (find c "-T:.Z")) ts)))))

;;; ---------------------------------------------------------------------
;;; Writing entries

(test audit-without-a-log-is-a-no-op
  (let ((splice:*audit-log* nil))
    (is (null (splice:audit :probe :n 1)))))

(test audit-returns-sequence-numbers
  (with-audit-log (path)
    (is (= 1 (splice:audit :probe :n 1)))
    (is (= 2 (splice:audit :probe :n 2)))))

(test an-entry-carries-its-envelope-and-fields
  (with-audit-log (path)
    (splice:audit :probe :n 7 :note (format nil "two~%lines"))
    (let ((entry (first (log-entries path))))
      (is (= 1 (getf entry :seq)))
      (is (string= "genesis" (getf entry :prev-hash)))
      (is (= 12 (length (getf entry :boot-id))))
      (is (eq :probe (getf entry :event)))
      (is (= 7 (getf entry :n)))
      (is (string= "two lines" (getf entry :note))))
    (is (= 1 (length (log-lines path))))))

(test each-entry-hashes-the-previous-line
  (with-audit-log (path)
    (splice:audit :probe :n 1)
    (splice:audit :probe :n 2)
    (destructuring-bind (first-line second-line) (log-lines path)
      (declare (ignore second-line))
      (is (string= (splice:sha256-hex first-line)
                   (getf (second (log-entries path)) :prev-hash))))))

;;; ---------------------------------------------------------------------
;;; The chain

(defun write-lines (path lines)
  (with-open-file (out path :direction :output :if-exists :supersede)
    (dolist (line lines) (write-line line out))))

(defun with-tampered-copy (path edit fn)
  "Call FN on a copy of the log at PATH whose lines went through EDIT."
  (let ((copy (temp-log-path)))
    (unwind-protect
         (progn (write-lines copy (funcall edit (log-lines path)))
                (funcall fn copy))
      (uiop:delete-file-if-exists copy))))

(defun three-entry-log ()
  (splice:audit :probe :n 1)
  (splice:audit :probe :n 2)
  (splice:audit :probe :n 3))

(test an-untouched-chain-verifies
  (with-audit-log (path)
    (three-entry-log)
    (is (equal '(t 3 nil) (multiple-value-list (splice:verify-audit-chain path))))))

(test an-edited-entry-breaks-the-next-link
  (with-audit-log (path)
    (three-entry-log)
    (with-tampered-copy
        path
        (lambda (lines)
          (list (first lines)
                (uiop:frob-substrings (second lines) '(":N 2") ":N 9")
                (third lines)))
      (lambda (copy)
        (is (equal '(nil 2 3) (multiple-value-list (splice:verify-audit-chain copy))))))))

(test a-deleted-entry-breaks-the-chain
  (with-audit-log (path)
    (three-entry-log)
    (with-tampered-copy path (lambda (lines) (list (first lines) (third lines)))
      (lambda (copy)
        (is (equal '(nil 1 3) (multiple-value-list (splice:verify-audit-chain copy))))))))

;;; ---------------------------------------------------------------------
;;; Reconstruction: the pure fold

(defun fold (&rest entries)
  (reduce #'splice::apply-audit-entry entries
          :initial-value (splice::empty-reconstruction)))

(defun registered (set-name &key functions classes)
  (list :event :patch-registered :patch-set set-name
        :functions (mapcar (lambda (f) (list :name f)) functions)
        :classes classes))

(defun event (kind set-name &rest fields)
  (list* :event kind :patch-set set-name fields))

(defun impl-of (state fname) (cdr (assoc fname (getf state :functions))))
(defun shape-of (state cname) (cdr (assoc cname (getf state :classes))))
(defun state-of (state set-name) (cdr (assoc set-name (getf state :sets))))

(test replace-entry-moves-the-name-to-the-front-once
  (is (equal '((b . 2) (a . 1) (c . 3))
             (splice::replace-entry '((a . 1) (b . 0) (c . 3)) 'b 2))))

(test folding-never-mutates-the-prior-state
  (let* ((before (fold (registered 's :functions '(f) :classes '(c))))
         (snapshot (copy-tree before)))
    (splice::apply-audit-entry before (event :install 's))
    (splice::apply-audit-entry before (event :promotion 's))
    (is (equal snapshot before))))

(test registration-records-the-set
  (let ((state (fold (registered 's :functions '(f)))))
    (is (eq :registered (state-of state 's)))
    ;; registration alone changes no code
    (is (null (impl-of state 'f)))))

(test install-shims-functions-and-unions-classes
  (let ((state (fold (registered 's :functions '(f g) :classes '(c))
                     (event :install 's))))
    (is (eq :staged (state-of state 's)))
    (is (eq :shimmed (impl-of state 'f)))
    (is (eq :shimmed (impl-of state 'g)))
    (is (eq :union (shape-of state 'c)))))

(test a-canary-transition-moves-only-the-set
  (let ((state (fold (registered 's :functions '(f))
                     (event :install 's)
                     (event :canary-transition 's :to :canary-25))))
    (is (eq :canary-25 (state-of state 's)))
    (is (eq :shimmed (impl-of state 'f)))))

(test promotion-points-at-the-new-code
  (let ((state (fold (registered 's :functions '(f) :classes '(c))
                     (event :install 's)
                     (event :promotion 's))))
    (is (eq :promoted (state-of state 's)))
    (is (equal '(:new s) (impl-of state 'f)))
    (is (eq :new (shape-of state 'c)))))

(test rollback-restores-the-originals
  (let ((state (fold (registered 's :functions '(f) :classes '(c))
                     (event :install 's)
                     (event :rollback-complete 's))))
    (is (eq :rolled-back (state-of state 's)))
    (is (eq :original (impl-of state 'f)))
    (is (eq :original (shape-of state 'c)))))

(test only-a-veto-verdict-vetoes
  (is (eq :vetoed (state-of (fold (registered 's)
                                  (event :contract-result 's :verdict :veto))
                            's)))
  (dolist (verdict '(:pass :fail))
    (is (eq :registered (state-of (fold (registered 's)
                                        (event :contract-result 's :verdict verdict))
                                  's)))))

(test freeze
  (is (eq :frozen (state-of (fold (registered 's) (event :install 's)
                                  (event :freeze 's))
                            's))))

(test other-events-leave-the-state-alone
  (let ((state (fold (registered 's :functions '(f)))))
    (is (eq state (splice::apply-audit-entry state (event :task-sample 's))))
    (is (eq state (splice::apply-audit-entry state (event :canary-anomaly 's))))))

(test the-latest-set-to-touch-a-function-wins
  (let ((state (fold (registered 'a :functions '(f))
                     (event :install 'a)
                     (event :rollback-complete 'a)
                     (registered 'b :functions '(f))
                     (event :install 'b)
                     (event :promotion 'b))))
    (is (equal '(:new b) (impl-of state 'f)))
    (is (eq :rolled-back (state-of state 'a)))
    (is (eq :promoted (state-of state 'b)))))

;;; ---------------------------------------------------------------------
;;; Reconstruction from a file

(test reconstruct-state-folds-a-written-log
  (with-audit-log (path)
    (splice:audit :patch-registered :patch-set 'rs :functions '((:name f)) :classes '(c))
    (splice:audit :install :patch-set 'rs)
    (splice:audit :promotion :patch-set 'rs)
    (let ((state (splice:reconstruct-state path)))
      (is (eq :promoted (state-of state 'rs)))
      (is (equal '(:new rs) (impl-of state 'f)))
      (is (eq :new (shape-of state 'c))))))

(test reconstruct-state-refuses-a-broken-chain
  (with-audit-log (path)
    (three-entry-log)
    (with-tampered-copy path (lambda (lines) (list (first lines) (third lines)))
      (lambda (copy)
        (signals error (splice:reconstruct-state copy))))))
