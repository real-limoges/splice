;;;; demo-cluster.lisp: cluster propagation prototype, exercised against
;;;; two real SBCL images.
;;;;
;;;;   sbcl --non-interactive --load examples/orders/scripts/demo-cluster.lisp
;;;;
;;;; Spawns two member SBCL processes, ships them the currency patch set as
;;;; source (two-phase prepare/commit, hash-verified), injects the latent
;;;; fault on the member at port 7802 only, and lets the conductor's
;;;; pooled-SPRT/local-reject aggregation roll back both members.
;;;;
;;;; Does not run yet: the TCP transport's RUN-MEMBER and RUN-CONDUCTOR are
;;;; not written.

(require :asdf)
(defvar *splice-asd* (merge-pathnames "../../../splice.asd" *load-truename*))
(defvar *orders-asd* (merge-pathnames "../splice-orders.asd" *load-truename*))
(asdf:load-asd *splice-asd*)
(asdf:load-asd *orders-asd*)
(asdf:load-system "splice/cluster-tcp")
(asdf:load-system "splice-orders")

(defpackage :splice-cluster-demo (:use :cl))
(in-package :splice-cluster-demo)

(defun spawn-member (port)
  (sb-ext:run-program
   "sbcl"
   (list "--non-interactive"
         "--eval" "(require :asdf)"
         "--eval" (format nil "(asdf:load-asd ~s)" (namestring (truename *splice-asd*)))
         "--eval" (format nil "(asdf:load-asd ~s)" (namestring (truename *orders-asd*)))
         "--eval" "(asdf:load-system \"splice/cluster-tcp\")"
         "--eval" "(asdf:load-system \"splice-orders\")"
         "--eval" (format nil "(splice/cluster:run-member :port ~a)" port))
   :search t :wait nil :output nil :error nil))

(format t "~%=== splice cluster demo: 1 conductor, 2 member SBCL images ===~%")
(format t "spawning members on ports 7801 (healthy) and 7802 (latent fault)...~%")

(let ((m1 (spawn-member 7801))
      (m2 (spawn-member 7802)))
  (unwind-protect
       (let ((finals (splice/cluster:run-conductor
                      '(("127.0.0.1" 7801)
                        ("127.0.0.1" 7802 :fault :latent)))))
         (format t "~%--- final member reports ---~%")
         (dolist (f finals)
           (format t "  member ~a: state=~a chain-ok=~a reconstruction=~a ~
mixed=~a~%             reversal=~a~%"
                   (getf f :port) (getf f :state) (getf f :chain-ok)
                   (getf f :reconstruction-ok) (getf f :mixed)
                   (getf f :reversal)))
         (format t "~%verdict:~%")
         (format t "  both members rolled back:  ~a~%"
                 (every (lambda (f) (eq (getf f :state) :rolled-back)) finals))
         (format t "  both audit chains intact:  ~a~%"
                 (every (lambda (f) (getf f :chain-ok)) finals))
         (format t "  both reconstructions ok:   ~a~%"
                 (every (lambda (f) (getf f :reconstruction-ok)) finals))
         (format t "  zero mixed executions:     ~a~%"
                 (every (lambda (f) (and (getf f :mixed) (zerop (getf f :mixed))))
                        finals))
         (multiple-value-bind (ok entry-count)
             (splice:verify-audit-chain "/tmp/splice-conductor.log")
           (format t "  conductor log intact:      ~a (~a entries)~%"
                   ok entry-count)))
    (ignore-errors (sb-ext:process-kill m1 15))
    (ignore-errors (sb-ext:process-kill m2 15))))

(format t "~%cluster demo complete.~%")
