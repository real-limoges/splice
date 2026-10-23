;;;; cluster-protocol.lisp: cluster protocol with no transport
;;;;
;;;; Blah blah blah

(in-package #'splice/cluster)

(defun canonical-text (value)
  "VALUE printed identically, whatever the caller's printer settings.
VALUE holds *only* keywords, strings, integers, and lists"
  (with-standard-io-syntax
    (let ((*print-pretty* nil))
      (prin1-to-string value))))

(defun spec-without-hash (spec)
  "SPEC's fields (without :HASH) in order"
  (loop for (key value) on spec by #'cddr
        unless (eq key :hash)
          append (list key value)))

(defun spec-hash (spec)
  "SHA-256 of SPEC's canonical text (without :HASH)"
  (splice:sha256-hex (canonical-text (spec-without-hash spec))))

(defun spec-with-hash (spec)
  "SPEC wiht :HASH set to its SPEC-HASH as the first field"
  (list* :hash (spec-hash spec) (spec-without-hash spec)))

(defun spec-field (spec key)
  "SPEC's KEY field. Errors without"
  (let ((value (getf spec key :missing)))
    (when (eq value :missing)
      (error "patch spec has no ~s field" key))
    value))

(defun spec-package (spec)
  "SPEC's names and source text"
  (or (find-package (spec-field spec :package))
      (error "spec package ~a does not exist in this image"
             (spec-field spec :package))))

(defun read-source (text package)
  "The one form TEXT holds, read from PACKAGE with *READ-EVAL* off.
Reading creates new symbols, as compiling the same source as a file"
  (with-standard-io-syntax
    (let ((*package* package)
          (*read-eval* nil))
      (read-from-string text))))

(defun existing-symbol (name package)
  "Symbol called NAME in the PACKAGE. A function, class, migration
or a dropped slot must exist a head of time. Missing one is a hard
error, not a fresh symbol"
  (multiple-value-bind (symbol status) (find-symbol name package)
    (unless status
      (error "~a names nothing in ~a" name (package-name package)))
    symbol))

;; Patches

(defun spec-fn-patch (entry package)
  "FN-PATCH from a spec's (NAME TEXT) function entry"
  (destructuring-bind (name text) entry
    (let ((form (read-source text package)))
      (splice::make-fn-patch :name (existing-symbol name package)
                             :new-fn (compile nil form)
                             :new-source form
                             :new-source-hash (splice:sha256-hex text)))))

(defun spec-class-patch (entry package)
  "CLASS-PATCH from a spec's class entry - plist of names and text"
  (destructuring-bind (&key name add-slots drop-slots
                         migrate-forward migrate-backward)
      entry
    (splice::make-class-patch
     :class-name (existing-symbol name package)
     :add-slots (read-source add-slots package)
     :drop-slots (mapcar (lambda (slot) (existing-symbol slot package))
                         drop-slots)
     :migrate-forward (fdefinition (existing-symbol migrate-forward package))
     :migrate-backward (fdefinition (existing-symbol migrate-backward package)))))

(defun spec-contract (entry package)
  "CONTRACT from a spec's (NAME TEXT). The name becomes a keyword since
it's just a label in the audit log."
  (destructuring-bind (name text) entry
    (let ((form (read-source text package)))
      (splice::make-contract :name (intern name :keyword)
                             :check-fn (compile nil form)
                             :source form))))

(defun build-patch-set (spec)
  "Check SPEC's hash, compile its source, and register the set.
Signals error + registers nothing when the hash mismatches, a missing name
or PATCH-VETOED when a static check refuses."
  (unless (equal (getf spec :hash) (spec-hash spec))
    (error "patch spec hash mismatch"))
  (let ((package (spec-package spec)))
    (splice:register-patch-set
     (splice::make-patch-set
      :name (intern (spec-field spec :name) package)
      :description (getf spec :description)
      :ramp (spec-field spec :ramp)
      :sprt-min-n (spec-field spec :min-n)
      :sprt-max-n (spec-field spec :max-n)
      :fn-patches (mapcar (lambda (entry) (spec-fn-patch entry package))
                          (getf spec :functions))
      :class-patches (mapcar (lambda (entry) (spec-class-patch entry package))
                             (getf spec :classes))
      :contracts (mapcar (lambda (entry) (spec-contract entry package))
                         (getf spec :contracts))))))
