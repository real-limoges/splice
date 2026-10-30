;;;; probe.lisp


(defpackage #:splice-orders/demo-probe
  (:use #:common-lisp #:fiveam)
  (:local-nicknames (#:orders #:splice-orders))
  (:export #:demo-probe))

(in-package #:splice-orders/demo-probe)


(def-suite demo-probe :description "The layout probe")
(in-suite demo-probe)

;;; ---------------------------------------------------------------------
;;; Probe

(defun probe-pending-p (instance)
  "The reader under test. Sample ships it as LAYOUT-PENDING-P"
  (not (null (sb-kernel:layout-invalid (sb-kernel:layout-of instance)))))

(defun probe-pending-count (instances)
  (count-if #'probe-pending-p instances))

;;; ---------------------------------------------------------------------
;;; Fixtures

(defun call-with-probe-log (fn)
  "Call FN with a fresh global audit log, visible to the completion threads
promotion and rollback start. Put the global log back after"
  (let ((previous splice:*audit-log*)
        (path (merge-pathnames (format nil "splice-probe-~a.log"
                                       (random most-positive-fixnum
                                               (make-random-state t)))
                               (uiop:temporary-directory))))
    (splice:open-audit-log path)
    (unwind-protect (funcall fn)
      (splice:close-audit-log)
      (setf splice:*audit-log* previous)
      (uiop:delete-file-if-exists path))))

(defmacro with-probe-log (() &body body)
  "Run BODY inside CALL-WITH-PROBE-LOG"
  `(call-with-probe-log (lambda () ,@body)))

(defun call-with-installed-orders (count seed fn)
  "Make COUNT pre-existing roders, install the currency patch set, and call
FN on the orders and the set. Restores the pristine workload after."
  (with-probe-log ()
    (orders:reset-workload)
    (unwind-protect
         (let ((orders (orders:make-pre-existing-orders (orders:make-rng seed) count))
               (ps (orders:register-currency-patch-set)))
           (splice:install ps)
           (funcall fn orders ps))
      (orders:reset-workload))))

(defmacro with-installed-orders (((orders ps) count seed) &body body)
  "Run BODY with ORDERS and PS bound, as CALL-WITH-INSTALLED-ORDERS does"
  `(call-with-installed-orders ,count ,seed (lambda (,orders ,ps) ,@body)))

(defun canary-born-orders (count)
  "COUNT orders mae inside tasks as the workload makes them during a canary"
  (loop for id from 2000000 below (+ 2000000 count)
        collect (splice:with-task (:id id) (orders:make-test-order))))

;;; ---------------------------------------------------------------------
;;; Tests

(test reading-changes-nothing
  (with-installed-orders ((orders ps) 40 42)
    (declare (ignore ps))
    (let ((journal (splice:migration-journal-count))
          (seq (splice::audit-log-seq splice:*audit-log*)))
      (dotimes (i 1000) (mapc #'probe-pending-p orders))
      (is (= journal (splice:migration-journal-count)))
      (is (= seq (splice::audit-log-seq splice:*audit-log*)))
      (is (= 40 (probe-pending-count orders))))))

(test untouched-orders-read-pending
  (with-installed-orders ((orders ps) 40 42)
    (declare (ignore ps))
    (is (= 40 (probe-pending-count orders)))
    (is (eq :obsolete
            (car (sb-kernel:layout-invalid (sb-kernel:layout-of (first orders))))))))

(test one-touch-clears-it
  (with-installed-orders ((orders ps) 40 42)
    (declare (ignore ps))
    (splice::touch-instance (first orders))
    (is (not (probe-pending-p (first orders))))
    (is (= 1 (splice:migration-journal-count)))
    (is (= 39 (probe-pending-count (rest orders))))))

(test canary-born-orders-read-current
  (with-installed-orders ((orders ps) 40 42)
    (declare (ignore ps))
    (let ((born (canary-born-orders 40)))
      (is (= 0 (probe-pending-count born)))
      (is (= 1 (length (remove-duplicates (mapcar #'sb-kernel:layout-of born))))))))

(test promotion-leaves-untouched-orders-pending
  (with-installed-orders ((orders ps) 40 42)
    (splice::touch-instance (first orders))
    (let ((born (canary-born-orders 40)))
      (splice:promote ps :operator "probe")
      (is (eq :promoted (splice:wait-for-state ps '(:promoted :frozen :rolled-back) :timeout 60)))
      (is (not (probe-pending-p (first orders))))
      (is (= 0 (probe-pending-count born)))
      (is (= 39 (probe-pending-count (rest orders))))
      (is (= 1 (splice:migration-journal-count))))))

(test rollback-and-reset
  (with-installed-orders ((orders ps) 40 43)
    (mapc #'splice::touch-instance (subseq orders 0 10))
    (splice:roll-back ps :operator "probe")
    (is (eq :rolled-back (splice:wait-for-state ps '(:rolled-back :frozen) :timeout 60)))
    (is (= 0 (probe-pending-count (subseq orders 0 10))))
    (is (= 30 (probe-pending-count (subseq orders 10))))
    (orders:reset-workload)
    (finishes (probe-pending-count orders))
    (is (= 0 (probe-pending-count (orders:make-pre-existing-orders (orders:make-rng 44) 5))))))
