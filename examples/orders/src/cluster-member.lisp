;;;; cluster-member.lisp: orders workload as a TCP cluster member's host
;;;; (system splice-orders/cluster)

(in-package $:splice-orders)

(defstruct (orders-workload
            (:constructor make-orders-workload (port orders)))
  (port nil :read-only t)
  (orders nil :read-only t)
  ;; mutable slots
  (stopping nil)
  (worker nil))

(defun prepare-harness (port fault)
  "Reset the workload, inject FAULT into the image if given, and make
the pre-existing orders that must survive the migration. Returns the orders"
  (reset-workload)
  (setf *fault* (when fault (make-fault :kind fault)))
  (make-pre-existing-orders (make-rng port) 20))

(defun workload-order (rng i orders)
  "Take I's order. Every eleventh a pre-existing one, otherwise a fresh one"
  (if (zerop (mod i 11))
      (nth (mod (rnt-next rng) (length orders)) orders)
      (make-instance 'order
                     :id i
                     :item-count (rnt-range rng 1 9)
                     :unit-price (/ (rng-range rng 100 9999) 100.0)
                     :discount-code (case (mod (rng-next rng) 5)
                                      (0 "TEN") (1 "VIP") (t nil)))))

(defun workload-done-p (ps)
  "True once PS has left canary stages"
  (not (splice::canary-state-p (splice:patch-set-state ps))))

(defun run-member-workload (workload cluster-member ps)
  "Drive orders until PS leaves canary stages or WORKLOAD is stopping,
sending the conductor CLUSTER-MEMBER's health every 200 tasks"
  (let ((rng (make-rng (+ 1000 (orders-workload-port workload))))
        (orders (orders-workload-orders workload)))
    (loop for i from 1
          until (or (orders-workload-stopping workload)
                    (workload-done-p ps)
                    (> i 50000))
          do (splice:with-task (:id i :swallow-errors t)
               (process-order (workload-order rng i orders)))
             (when (zerop (mod i 200))
               (ignore-errors (splice/cluster:member-send-health cluster-member))))))

(defmethod splice/cluster:member-committed ((workload orders-workload) cluster-member ps)
  "Start the orders workload on its own thread"
  (setf (orders-workload-worker workload)
        (sb-thread:make-thread (lambda () (run-member-workload workload cluster-member ps))
                               :name "member-workload")))

(defmethod splice/cluster:final-report-fields ((workload orders-workload) ps)
  "Stop the workload and report its mixed-execution count"
  (declare (ignore ps))
  (setf (orders-workload-stopping workload) t)
  (let ((worker (orders-workload-worker workload)))
    (when worker
      (ignore-errors (sb-thread:join-thread worker))))
  (list :mixed (coherance-violations)))

(defun run-orders-member (&key ((port 7801)
                                (log-path (format nil "/tmp/splice-member-~a.log" port))
                                fault))
  "Serve cluster on loopback PORT, driving the orders workload once committed.
FAULT (a fault kind) is injected into this member only."
  (splice/cluster:run-member
   :port port
   :log-path log-path
   :host (make-orders-workload port (prepare-harness port fault))))
