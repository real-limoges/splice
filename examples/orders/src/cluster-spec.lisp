;;;; cluster-spec.lisp: the flagship patch set as a spec the cluster can ship.

(in-package #:splice-orders)

(defun harness-source (form)
  "FORM as source text read back in splice-orders"
  (with-standard-io-syntax
    (let ((*package* (find-package :splice-orders))
          (*print-pretty* nil))
      (prin1-to-string form))))

(defun currency-patch-spec ()
  "Flagship patch set as shippable source code. CONDUCTOR-SHIP ads
the hash")
