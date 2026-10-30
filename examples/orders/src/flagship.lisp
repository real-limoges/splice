;;;; flagship.lisp: the flagship patch set. It moves order math from float
;;;; dollars to rational minor units (three functions, a method, and a class
;;;; redefinition). New code writes through to FLOAT-TOTAL so old code stays
;;;; correct during canary.

(in-package #:splice-orders)

;;; ---------------------------------------------------------------------
;;; New order math

(defun write-through-float (order cents)
  "Keep FLOAT-TOTAL in sync while it exists (the canary union shape)."
  (when (slot-exists-p order 'float-total)
    (setf (slot-value order 'float-total) (/ cents 100.0))))

(defun new-price-order (order)
  (maybe-inject :price order (if *fault-rng* (rng-uniform *fault-rng*) 0.5))
  (let ((cents (round (* 100 (order-item-count order)
                         (rational (order-unit-price order))))))
    (setf (slot-value order 'amount-minor-units) cents
          (order-status order) :priced)
    (write-through-float order cents)
    cents))

(defun new-apply-discount (order)
  (maybe-inject :discount order 0.5)
  (let ((cents (round (* (slot-value order 'amount-minor-units)
                         (discount-multiplier (order-discount-code order))))))
    (setf (slot-value order 'amount-minor-units) cents
          (order-status order) :discounted)
    (write-through-float order cents)
    cents))

(defun new-total-invoice (order)
  (maybe-inject :invoice order 0.5)
  (let ((cents (slot-value order 'amount-minor-units)))
    (setf (order-status order) :invoiced)
    (write-through-float order cents)
    cents))

;;; ---------------------------------------------------------------------
;;; Class migration

(defun forward-migration (instance old-values)
  (when (and (fault-active-p :migration)
             (let ((ft (getf old-values 'float-total)))
               (and ft (minusp ft))))
    (error "migration fault: cannot derive minor units from corrupted total ~a"
           (getf old-values 'float-total)))
  (setf (slot-value instance 'amount-minor-units)
        (round (* 100 (rational (or (getf old-values 'float-total) 0))))))

(defun backward-migration (instance new-values)
  (setf (slot-value instance 'float-total)
        (/ (or (getf new-values 'amount-minor-units) 0) 100.0)))

;;; ---------------------------------------------------------------------
;;; Patch set

(defun make-test-order (&key (cents 1099) (code nil))
  (make-instance 'order :id -1 :item-count 1 :unit-price (/ cents 100.0)
                        :discount-code code))

(defun register-currency-patch-set (&key (ramp '(5 25 100)) (alpha 0.01d0)
                                      (beta 0.05d0) (min-n 50) (max-n 2000)
                                      paired-fallback lossy)
  "Register a fresh flagship patch set. Faults come from *FAULT*, so one set
serves every session."
  (splice:unregister-patch-set 'currency-migration)
  (splice:register-patch-set
   (splice::make-patch-set
    :name 'currency-migration
    :description "orders: float dollars -> rational minor units"
    :ticket "SPLICE-42"
    :ramp ramp :alpha alpha :beta beta
    :sprt-min-n min-n :sprt-max-n max-n
    :paired-fallback paired-fallback
    :fn-patches
    (list (splice::make-fn-patch
           :name 'price-order :new-fn #'new-price-order
           :new-source '(function new-price-order)
           :new-source-hash (splice:sha256-hex "new-price-order-v2"))
          (splice::make-fn-patch
           :name 'apply-discount :new-fn #'new-apply-discount
           :new-source '(function new-apply-discount)
           :new-source-hash (splice:sha256-hex "new-apply-discount-v2"))
          (splice::make-fn-patch
           :name 'total-invoice :new-fn #'new-total-invoice
           :new-source '(function new-total-invoice)
           :new-source-hash (splice:sha256-hex "new-total-invoice-v2")))
    :method-patches
    (list (splice::make-method-patch
           :gf-name 'print-total :specializer-names '(order stream)
           :new-fn (lambda (order stream)
                     (multiple-value-bind (dollars cents)
                         (floor (slot-value order 'amount-minor-units) 100)
                       (format stream "$~d.~2,'0d" dollars cents)))
           :new-source '(print-total-minor-units)
           :new-source-hash (splice:sha256-hex "print-total-v2")))
    :class-patches
    (list (splice::make-class-patch
           :class-name 'order
           :add-slots '((amount-minor-units :initform 0))
           :drop-slots '(float-total)
           :migrate-forward #'forward-migration
           :migrate-backward #'backward-migration
           :lossy-p lossy))
    :contracts
    (list (splice::make-contract
           :name 'invoice-is-exact
           :check-fn (lambda ()
                       (let ((o (make-test-order :cents 1099))
                             (*fault* nil)) ; contracts test the code, not the fault
                         (funcall (splice:new 'price-order) o)
                         (= 1099 (funcall (splice:new 'total-invoice) o))))
           :source '(invoice-is-exact))
          (splice::make-contract
           :name 'pipeline-conserves-discount
           :check-fn (lambda ()
                       (let ((o (make-test-order :cents 1000 :code "TEN"))
                             (*fault* nil))
                         (funcall (splice:new 'price-order) o)
                         (funcall (splice:new 'apply-discount) o)
                         (= 900 (funcall (splice:new 'total-invoice) o))))
           :source '(pipeline-conserves-discount))
          (splice::make-contract
           :name 'old-new-differential
           :check-fn (lambda ()
                       (let ((o1 (make-test-order :cents 2499 :code "VIP"))
                             (o2 (make-test-order :cents 2499 :code "VIP"))
                             (*fault* nil))
                         (price-order o1) (apply-discount o1) (total-invoice o1)
                         (funcall (splice:new 'price-order) o2)
                         (funcall (splice:new 'apply-discount) o2)
                         (<= (abs (- (round (* 100 (order-float-total o1)))
                                     (funcall (splice:new 'total-invoice) o2)))
                             1)))
           :source '(old-new-differential))))))
