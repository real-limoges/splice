;;;; gate.lisp

(in-package #:splice-orders/demo)

(defstruct gate
  (lock (sb-thread:make-mutex :name "demo-gate"))
  (queue (sb-thread:make-waitqueue))
  (opened '()))                         ; keys opened but not consumed

(defun open-gate (gate key)
  "Open KEY once. Exactly one waiter on KEY (present or future) gets through
Returns KEY"
  (sb-thread:with-mutex ((gate-lock gate))
    (push key (gate-opened gate))
    (sb-thread:condition-broadcast (gate-queue gate))
    key))

(defun await-gate (gate key)
  "Block until KEY is opened, then consume it. Returns KEY"
  (sb-thread:with-mutex ((gate-lock gate))
    (loop until (member key (gate-opened gate))
          do (sb-thread:condition-wait (gate-queue gate) (gate-lock gate)))
    (setf (gate-opened gate) (remove key (gate-opened gate) :count 1))
    key))
