;;;; migration.lisp: slot specs and class shapes, the pure end of class
;;;; migration.

(in-package #:splice-tests)

(def-suite migration :in splice-tests)
(in-suite migration)

(defclass shape-base () ())
(defclass shape-probe (shape-base)
  ((p :initarg :p :reader p-of)
   (q :initform 5 :accessor q-of)
   (r)))

(defun names-of (specs)
  (mapcar (lambda (spec) (getf spec :name)) specs))

(defun spec-named (name specs)
  (find name specs :key (lambda (spec) (getf spec :name))))

;;; Slot specs

(test a-bare-symbol-is-a-slot-with-defaults
  (is (equal '(:name x :initform nil :initfunction nil :initargs () :readers ()
               :writers () :type t)
             (splice::canonicalize-slot-spec 'x))))

(test every-option-lands-in-the-canonical-plist
  (let ((spec (splice::canonicalize-slot-spec
               '(x :initform (+ 1 2) :initarg :x :initarg :ex
                   :accessor x-of :type integer))))
    (is (eq 'x (getf spec :name)))
    (is (equal '(+ 1 2) (getf spec :initform)))
    (is (= 3 (funcall (getf spec :initfunction))))
    (is (equal '(:x :ex) (getf spec :initargs)))
    (is (equal '(x-of) (getf spec :readers)))
    (is (equal '((setf x-of)) (getf spec :writers)))
    (is (eq 'integer (getf spec :type)))))

(test readers-and-writers-accumulate-in-order
  (let ((spec (splice::canonicalize-slot-spec
               '(x :reader r1 :reader r2 :writer w1))))
    (is (equal '(r1 r2) (getf spec :readers)))
    (is (equal '(w1) (getf spec :writers)))))

;;; Union and final shapes

(defun class-patch-over-old (&key add drop)
  (splice::make-class-patch
   :class-name 'shape-probe
   :old-slot-specs (mapcar #'splice::canonicalize-slot-spec '(a (b :initform 1)))
   :add-slots add
   :drop-slots drop))

(test the-union-adds-new-slots-and-keeps-dropped-ones
  (let ((cp (class-patch-over-old :add '(c) :drop '(b))))
    (is (equal '(a b c) (names-of (splice::class-patch-union-slots cp))))))

(test the-final-shape-drops-slots
  (let ((cp (class-patch-over-old :add '(c) :drop '(b))))
    (is (equal '(a c) (names-of (splice::class-patch-final-slots cp))))))

(test an-added-slot-never-replaces-an-old-one
  (let* ((cp (class-patch-over-old :add '((b :initform 99) c)))
         (union (splice::class-patch-union-slots cp)))
    (is (equal '(a b c) (names-of union)))
    (is (eql 1 (getf (spec-named 'b union) :initform)))))

;;; Capturing a live class

(test capture-class-shape-reads-the-direct-slots
  (multiple-value-bind (slots supers) (splice::capture-class-shape 'shape-probe)
    (is (equal '(p q r) (names-of slots)))
    (is (equal '(shape-base) supers))
    (is (equal '(:p) (getf (spec-named 'p slots) :initargs)))
    (is (equal '(p-of) (getf (spec-named 'p slots) :readers)))
    (is (eql 5 (getf (spec-named 'q slots) :initform)))
    (is (equal '((setf q-of)) (getf (spec-named 'q slots) :writers)))))

(test snapshot-slots-skips-unbound-slots
  (let ((instance (make-instance 'shape-probe :p 1)))
    (is (equal '(p 1 q 5)
               (splice::snapshot-slots instance
                                       (splice::capture-class-shape 'shape-probe))))))
