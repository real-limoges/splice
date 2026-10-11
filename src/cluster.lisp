;;;; cluster.lisp: distributed patch propagation prototype.
;;;;
;;;; Topology: one conductor, N member images, sexp-over-TCP. Two-phase
;;;; propagation: PREPARE ships the patch spec (source forms + expected
;;;; hash); each member independently compiles, registers, and runs static
;;;; checks. Any veto aborts the cluster. COMMIT starts each member's local
;;;; canary (contract gate + ramp). Members stream SPRT health; the
;;;; conductor pools log-likelihood ratios (LLRs are sums over observations,
;;;; so per-node LLRs add) and broadcasts rollback on either a local reject
;;;; or a pooled reject. Safety never waits on the network for local hard
;;;; triggers, which roll back locally first and then report.
;;;;
;;;; Prototype limitations:
;;;; - contract checks run at COMMIT (inside INSTALL), not PREPARE; PREPARE
;;;;   covers compile + static checks.
;;;; - members ramp autonomously (a "ramp of ramps" cohort policy is not
;;;;   implemented); the conductor gates nothing but rollback.
;;;; - partition handling is not exercised (single host, no link failures).

(in-package #:splice/cluster)

(defvar *wire-lock* (sb-thread:make-mutex))

(defun send-form (stream form)
  (sb-thread:with-mutex (*wire-lock*)
    (let ((*package* (find-package :splice/cluster))
          (*print-pretty* nil))
      (prin1 form stream)
      (terpri stream)
      (force-output stream))))

(defun read-form (stream)
  (let ((*package* (find-package :splice/cluster)))
    (read stream nil :eof)))

;;; ---------------------------------------------------------------------
;;; The wire format of a patch set: source forms + integrity hash

(defun currency-patch-spec ()
  "The flagship patch set as shippable source. The conductor computes the
hash over the printed forms; members recompute and refuse a mismatch."
  (let ((functions
          '((price-order
             (lambda (order)
               (splice/harness::maybe-inject :price order
                (if splice/harness::*fault-rng*
                    (splice/harness::rng-uniform splice/harness::*fault-rng*)
                    0.5))
               (let ((cents (round (* 100 (splice/harness:order-item-count order)
                                       (rational (splice/harness:order-unit-price order))))))
                 (setf (slot-value order 'splice/harness::amount-minor-units) cents
                       (splice/harness:order-status order) :priced)
                 (splice/harness::write-through-float order cents)
                 cents)))
            (apply-discount
             (lambda (order)
               (splice/harness::maybe-inject :discount order 0.5)
               (let ((cents (round (* (slot-value order 'splice/harness::amount-minor-units)
                                      (splice/harness::discount-multiplier
                                       (splice/harness:order-discount-code order))))))
                 (setf (slot-value order 'splice/harness::amount-minor-units) cents
                       (splice/harness:order-status order) :discounted)
                 (splice/harness::write-through-float order cents)
                 cents)))
            (total-invoice
             (lambda (order)
               (splice/harness::maybe-inject :invoice order 0.5)
               (let ((cents (slot-value order 'splice/harness::amount-minor-units)))
                 (setf (splice/harness:order-status order) :invoiced)
                 (splice/harness::write-through-float order cents)
                 cents))))))
    (list :name 'currency-migration-cluster
          :functions functions
          :class '(:name order
                   :add-slots ((amount-minor-units :initform 0))
                   :drop-slots (float-total))
          :ramp '(5 25 100) :min-n 30 :max-n 2000
          :hash (splice:sha256-hex (prin1-to-string functions)))))

(defun build-patch-set-from-spec (spec)
  "Member side: verify integrity, compile the shipped sources, assemble the
patch set against local definitions."
  (let ((functions (getf spec :functions)))
    (unless (equal (getf spec :hash)
                   (splice:sha256-hex (prin1-to-string functions)))
      (error "patch spec hash mismatch; refusing to compile"))
    (splice:register-patch-set
     (splice::make-patch-set
      :name (getf spec :name)
      :description "cluster-shipped currency migration"
      :ramp (getf spec :ramp)
      :sprt-min-n (getf spec :min-n) :sprt-max-n (getf spec :max-n)
      :fn-patches
      (loop for (fname form) in functions
            collect (splice::make-fn-patch
                     :name (intern (symbol-name fname) :splice/harness)
                     :new-fn (compile nil form)
                     :new-source form
                     :new-source-hash (splice:sha256-hex (prin1-to-string form))))
      :class-patches
      (let ((c (getf spec :class)))
        (list (splice::make-class-patch
               :class-name (intern (symbol-name (getf c :name)) :splice/harness)
               :add-slots (sublis '((amount-minor-units
                                     . splice/harness::amount-minor-units))
                                  (getf c :add-slots))
               :drop-slots (list 'splice/harness::float-total)
               :migrate-forward #'splice/harness::forward-migration
               :migrate-backward #'splice/harness::backward-migration)))
      :contracts
      (list (splice::make-contract
             :name 'cluster-invoice-exact
             :check-fn (lambda ()
                         (let ((o (splice/harness:make-test-order :cents 1099))
                               (splice/harness:*fault* nil))
                           (funcall (splice:new 'splice/harness::price-order) o)
                           (= 1099 (funcall (splice:new 'splice/harness::total-invoice) o))))
             :source '(cluster-invoice-exact)))))))

;;; ---------------------------------------------------------------------
;;; Member

(defun run-member (&key (port 7801) (log-path (format nil "/tmp/splice-member-~a.log" port)))
  "Serve one conductor connection; PREPARE/COMMIT/ROLLBACK/REPORT protocol.
Drives its own synthetic workload once committed."
  (let ((server (make-instance 'sb-bsd-sockets:inet-socket
                               :type :stream :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address server) t)
    (sb-bsd-sockets:socket-bind server #(127 0 0 1) port)
    (sb-bsd-sockets:socket-listen server 1)
    (splice:open-audit-log log-path)
    (let* ((conn (sb-bsd-sockets:socket-accept server))
           (stream (sb-bsd-sockets:socket-make-stream conn :input t :output t))
           (ps nil) (worker nil) (stop (list nil)) (orders nil))
      (loop
        (let ((msg (read-form stream)))
          (when (eq msg :eof) (return))
          (ecase (first msg)
            (:prepare
             (destructuring-bind (&key spec fault) (rest msg)
               (splice/harness:reset-workload)
               (setf splice/harness:*fault*
                     (when fault (splice/harness:make-fault :kind fault)))
               ;; the pre-existing population: instances that live through
               ;; forward migration and must journal-exact restore on rollback
               (setf orders (splice/harness:make-pre-existing-orders
                             (splice/harness:make-rng port) 20))
               (handler-case
                   (progn (setf ps (build-patch-set-from-spec spec))
                          (send-form stream (list :prepared :name (splice:patch-set-name ps))))
                 (error (e)
                   (send-form stream (list :veto :reason (princ-to-string e)))))))
            (:commit
             (handler-case
                 (progn
                   (splice:install ps)
                   (setf worker (start-member-workload ps stream stop port orders))
                   (send-form stream (list :committed)))
               (splice:patch-vetoed (e)
                 (send-form stream (list :veto :reason (princ-to-string e))))))
            (:rollback
             (sb-thread:with-mutex ((splice::patch-set-lock ps))
               (when (splice::canary-stage-p (splice:patch-set-state ps))
                 (splice::initiate-rollback-locked ps '(:conductor-command))))
             (send-form stream (list :rolling-back)))
            (:report
             (setf (car stop) t)
             (when worker (ignore-errors (sb-thread:join-thread worker)))
             (splice:wait-for-state ps '(:promoted :rolled-back :frozen :vetoed)
                                    :timeout 30)
             (splice:close-audit-log)
             (multiple-value-bind (ok entry-count) (splice:verify-audit-chain log-path)
               (send-form stream
                          (list :final
                                :state (splice:patch-set-state ps)
                                :chain-ok ok
                                :entries entry-count
                                :mixed (splice/harness:coherence-violations)
                                :reconstruction-ok
                                (and (splice:verify-reconstruction log-path) t)
                                :reversal (splice::patch-set-last-reversal-report ps)))))
            (:bye
             (return)))))
      (ignore-errors (close stream))
      (sb-bsd-sockets:socket-close server))))

(defun start-member-workload (ps stream stop port orders)
  "Drive tasks until the set reaches a decision, streaming SPRT health to
the conductor every 200 tasks."
  (sb-thread:make-thread
   (lambda ()
     (let ((rng (splice/harness:make-rng (+ 1000 port))))
       (loop for i from 1
             until (or (car stop)
                       (member (splice:patch-set-state ps)
                               '(:promoted :rolled-back :frozen
                                 :promoting :rolling-back))
                       (> i 50000))
             do (splice:with-task (:id i :swallow-errors t)
                  (splice/harness:process-order
                   (if (zerop (mod i 11))
                       (nth (mod (splice/harness:rng-next rng) (length orders))
                            orders)
                       (make-instance 'splice/harness:order
                        :id i
                        :item-count (splice/harness:rng-range rng 1 9)
                        :unit-price (/ (splice/harness:rng-range rng 100 9999) 100.0)
                        :discount-code (case (mod (splice/harness:rng-next rng) 5)
                                         (0 "TEN") (1 "VIP") (t nil))))))
                (when (zerop (mod i 200))
                  (let ((sprt (splice::patch-set-sprt-error ps)))
                    (ignore-errors
                     (send-form stream
                                (list :health
                                      :state (splice:patch-set-state ps)
                                      :fraction (splice::patch-set-current-fraction ps)
                                      :n (splice:sprt-n sprt)
                                      :llr (float (splice:sprt-llr sprt) 1.0)))))))
       (splice:wait-for-state ps '(:promoted :rolled-back :frozen) :timeout 30)
       (ignore-errors
        (send-form stream (list :terminal :state (splice:patch-set-state ps))))))
   :name "member-workload"))

;;; ---------------------------------------------------------------------
;;; Conductor

(defstruct member-state host port stream socket thread
  (latest-llr 0.0) (latest-n 0) (state :unknown) (terminal nil) (final nil))

(defun connect-member (host port &key (retries 40))
  (loop for attempt from 1 do
    (handler-case
        (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                                     :type :stream :protocol :tcp)))
          (sb-bsd-sockets:socket-connect socket #(127 0 0 1) port)
          (return (make-member-state
                   :host host :port port :socket socket
                   :stream (sb-bsd-sockets:socket-make-stream
                            socket :input t :output t))))
      (sb-bsd-sockets:socket-error (e)
        (when (>= attempt retries) (error e))
        (sleep 0.25)))))

(defun run-conductor (members-spec &key (alpha 0.01d0) (beta 0.05d0)
                                        (log-path "/tmp/splice-conductor.log"))
  "MEMBERS-SPEC: list of (host port &key fault). Returns the final reports.
Implements pooled-SPRT aggregation: cluster LLR = sum of member LLRs, tested
against the same Wald thresholds; any member's local rollback also triggers
a cluster-wide rollback."
  (splice:open-audit-log log-path)
  (let* ((spec (currency-patch-spec))
         (upper (log (/ (- 1d0 beta) alpha)))
         (members (mapcar (lambda (m)
                            (destructuring-bind (host port &key fault) m
                              (let ((ms (connect-member host port)))
                                (setf (getf (member-state-final ms) :fault) fault)
                                ms)))
                          members-spec))
         (mailbox '()) (mailbox-lock (sb-thread:make-mutex)))
    (splice:audit :cluster-prepare :members (mapcar #'second members-spec)
                  :spec-hash (getf spec :hash))
    ;; PREPARE on every member; any veto aborts the cluster
    (loop for ms in members
          for (nil nil . opts) in members-spec
          do (send-form (member-state-stream ms)
                        (list :prepare :spec spec :fault (getf opts :fault)))
             (let ((reply (read-form (member-state-stream ms))))
               (unless (eq (first reply) :prepared)
                 (splice:audit :cluster-abort :reason reply)
                 (error "member ~a vetoed prepare: ~a"
                        (member-state-port ms) reply))))
    ;; COMMIT everywhere; then reader threads feed the mailbox
    (dolist (ms members)
      (send-form (member-state-stream ms) '(:commit))
      (let ((reply (read-form (member-state-stream ms))))
        (unless (eq (first reply) :committed)
          (error "member ~a failed commit: ~a" (member-state-port ms) reply))))
    (splice:audit :cluster-commit :members (mapcar #'member-state-port members))
    (dolist (ms members)
      (setf (member-state-thread ms)
            (sb-thread:make-thread
             (lambda ()
               (loop for msg = (ignore-errors (read-form (member-state-stream ms)))
                     until (or (null msg) (eq msg :eof))
                     do (sb-thread:with-mutex (mailbox-lock)
                          (push (cons ms msg) mailbox))
                     until (eq (first msg) :terminal)))
             :name (format nil "conductor-reader-~a" (member-state-port ms)))))
    ;; aggregation loop
    (let ((rollback-sent nil))
      (loop
        (sleep 0.05)
        (let ((msgs (sb-thread:with-mutex (mailbox-lock)
                      (prog1 (reverse mailbox) (setf mailbox '())))))
          (dolist (entry msgs)
            (destructuring-bind (ms . msg) entry
              (case (first msg)
                (:health
                 (destructuring-bind (&key state fraction n llr) (rest msg)
                   (declare (ignore fraction))
                   (setf (member-state-latest-llr ms) llr
                         (member-state-latest-n ms) n
                         (member-state-state ms) state)))
                (:terminal
                 (setf (member-state-terminal ms) (getf (rest msg) :state)
                       (member-state-state ms) (getf (rest msg) :state))))))
          ;; pooled verdict: per-node LLRs add
          (let ((pooled (reduce #'+ members :key #'member-state-latest-llr))
                (local-reject (find :rolled-back members
                                    :key #'member-state-terminal))
                (local-rolling (find :rolling-back members
                                     :key #'member-state-state)))
            (when (and (not rollback-sent)
                       (or local-reject local-rolling (>= pooled upper)))
              (setf rollback-sent t)
              (splice:audit :cluster-rollback
                            :pooled-llr (float pooled 1.0) :upper (float upper 1.0)
                            :trigger (cond (local-reject :member-local-reject)
                                           (local-rolling :member-local-reject)
                                           (t :pooled-sprt-reject)))
              (dolist (ms members)
                (ignore-errors (send-form (member-state-stream ms) '(:rollback))))))
          (when (every #'member-state-terminal members)
            (return)))))
    ;; collect final reports and reconcile
    (let ((finals
            (mapcar (lambda (ms)
                      (ignore-errors (sb-thread:join-thread (member-state-thread ms)))
                      (send-form (member-state-stream ms) '(:report))
                      (let ((final (loop for msg = (read-form (member-state-stream ms))
                                         when (eq msg :eof) return msg
                                         when (eq (first msg) :final) return msg)))
                        (ignore-errors
                         (send-form (member-state-stream ms) '(:bye)))
                        (ignore-errors (close (member-state-stream ms)))
                        (if (eq final :eof)
                            (list :port (member-state-port ms) :state :died)
                            (list* :port (member-state-port ms) (rest final)))))
                    members)))
      (splice:audit :cluster-final :reports finals)
      (splice:close-audit-log)
      finals)))
