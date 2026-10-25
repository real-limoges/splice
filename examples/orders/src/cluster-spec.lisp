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
the hash"
  (list
   :name "CURRENCY-MIGRATION-CLUSTER"
   :package "SPLICE-ORDERS"
   :description "cluster-shipped currency migration"
   :functions
   (list
    (list "PRICE-ORDER"
          (harness-source
           '(lambda (order)
             (splice-orders::maybe-inject :price order
              (if splice-orders::*fault-rng*
                  (splice-orders::rng-uniform splice-orders::*fault-rng*)
                  0.5))
             (let ((cents (round (* 100 (splice-orders:order-item-count order)
                                    (rational (splice-orders:order-unit-price order))))))
               (setf (slot-value order 'splice-orders::amount-minor-units) cents
                     (splice-orders:order-status order) :priced)
               (splice-orders::write-through-float order cents)
               cents))))
    (list "APPLY-DISCOUNT"
          (harness-source
           '(lambda (order)
             (splice-orders::maybe-inject :discount order 0.5)
             (let ((cents (round (* (slot-value order 'splice-orders::amount-minor-units)
                                    (splice-orders::discount-multiplier
                                     (splice-orders:order-discount-code order))))))
               (setf (slot-value order 'splice-orders::amount-minor-units) cents
                     (splice-orders:order-status order) :discounted)
               (splice-orders::write-through-float order cents)
               cents))))
    (list "TOTAL-INVOICE"
          (harness-source
           '(lambda (order)
             (splice-orders::maybe-inject :invoice order 0.5)
             (let ((cents (slot-value order 'splice-orders::amount-minor-units)))
               (setf (splice-orders:order-status order) :invoiced)
               (splice-orders::write-through-float order cents)
               cents)))))
    :classes
    (list (list :name "ORDER"
                :add-slots (harness-source
                            '((splice-orders::amount-minor-units :initform 0)))
                :drop-slots '("FLOAT-TOTAL")
                :migrate-forward "FORWARD-MIGRATION"
                :migrate-backward "BACKWARD-MIGRATION"))
    :contracts
    (list (list "CLUSTER-INVOICE-EXACT"
                (harness-source
                 '(lambda ()
                   (let ((o (splice-orders:make-test-order :cents 1099))
                         (splice-orders:*fault* nil))
                     (funcall (splice:new 'splice-orders::price-order) o)
                     (= 1099 (funcall (splice:new 'splice-orders::total-invoice) o)))))))
   :ramp '(5 25 100)
   :min-n 30
   :max-n 2000))
