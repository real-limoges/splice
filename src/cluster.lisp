;;;; cluster.lisp: distributed patch propagation prototype.
;;;;
;;;; One conductor, N member images, sexps over TCP. PREPARE ships source
;;;; forms plus a hash; each member compiles and runs static checks, and any
;;;; veto aborts the cluster. COMMIT starts each member's local canary.
;;;; Members stream SPRT health; the conductor sums their LLRs and broadcasts
;;;; rollback on a local or pooled reject. Local hard triggers roll back
;;;; first and report after, never waiting on the network.
;;;;
;;;; Limitations:
;;;; - contracts run at COMMIT (inside INSTALL), not PREPARE.
;;;; - members ramp independently; the conductor gates only rollback.
;;;; - no partition handling (single host, no link failures).

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
  "The flagship patch set as shippable source, hashed over its printed
forms so members can refuse a mismatch."
  (let ((functions
          '((price-order
             (lambda (order)
               (h::maybe-inject :price order
                                (if h::*fault-rng*
                                    (h::rng-uniform h::*fault-rng*)
                                    0.5))
               (let ((cents (round (* 100 (h:order-item-count order)
                                      (rational (h:order-unit-price order))))))
                 (setf (slot-value order 'h::amount-minor-units) cents
                       (h:order-status order) :priced)
                 (h::write-through-float order cents)
                 cents)))
            (apply-discount
             (lambda (order)
               (h::maybe-inject :discount order 0.5)
               (let ((cents (round (* (slot-value order 'h::amount-minor-units)
                                      (h::discount-multiplier
                                       (h:order-discount-code order))))))
                 (setf (slot-value order 'h::amount-minor-units) cents
                       (h:order-status order) :discounted)
                 (h::write-through-float order cents)
                 cents)))
            (total-invoice
             (lambda (order)
               (h::maybe-inject :invoice order 0.5)
               (let ((cents (slot-value order 'h::amount-minor-units)))
                 (setf (h:order-status order) :invoiced)
                 (h::write-through-float order cents)
                 cents))))))
    (list :name 'currency-migration-cluster
          :functions functions
          :class '(:name order
                   :add-slots ((amount-minor-units :initform 0))
                   :drop-slots (float-total))
          :ramp '(5 25 100) :min-n 30 :max-n 2000
          :hash (splice:sha256-hex (prin1-to-string functions)))))

(defun build-patch-set-from-spec (spec)
  "Verify the hash, compile the shipped sources, and register the set."
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
                                     . h::amount-minor-units))
                                  (getf c :add-slots))
               :drop-slots (list 'h::float-total)
               :migrate-forward #'h::forward-migration
               :migrate-backward #'h::backward-migration)))
      :contracts
      (list (splice::make-contract
             :name 'cluster-invoice-exact
             :check-fn (lambda ()
                         (let ((o (h:make-test-order :cents 1099))
                               (h:*fault* nil))
                           (funcall (splice:new 'h::price-order) o)
                           (= 1099 (funcall (splice:new 'h::total-invoice) o))))
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
               (h:reset-workload)
               (setf h:*fault*
                     (when fault (h:make-fault :kind fault)))
               ;; orders that must survive forward migration and restore on rollback
               (setf orders (h:make-pre-existing-orders
                             (h:make-rng port) 20))
               (handler-case
                   (progn (setf ps (build-patch-set-from-spec spec))
                          (send-form stream (list :prepared :name (splice:patch-set-name ps))))
                 (error (e)
                   (send-form stream (list :veto :reason (princ-to-string e)))))))
            (:commit
             (handler-case
                 (progn
                   (splice:install ps)
                   (send-form stream (list :committed))
                   (setf worker (start-member-workload ps stream stop port orders)))
               (splice:patch-vetoed (e)
                 (send-form stream (list :veto :reason (princ-to-string e))))))
            (:rollback
             (send-form stream
                        (list (splice::with-set-lock (ps)
                                (if (splice::canary-stage-p (splice:patch-set-state ps))
                                    (splice::initiate-rollback-locked
                                     ps '(:conductor-command))
                                    :continue)))))
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
                                :mixed (h:coherence-violations)
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
     (let ((rng (h:make-rng (+ 1000 port))))
       (loop for i from 1
             until (or (car stop)
                       (member (splice:patch-set-state ps)
                               '(:promoted :rolled-back :frozen
                                 :promoting :swapping :rolling-back :restoring))
                       (> i 50000))
             do (splice:with-task (:id i :swallow-errors t)
                  (h:process-order
                   (if (zerop (mod i 11))
                       (nth (mod (h:rng-next rng) (length orders))
                            orders)
                       (make-instance 'h:order
                                      :id i
                                      :item-count (h:rng-range rng 1 9)
                                      :unit-price (/ (h:rng-range rng 100 9999) 100.0)
                                      :discount-code (case (mod (h:rng-next rng) 5)
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
  "MEMBERS-SPEC is a list of (host port &key fault). Rolls back the cluster
when the summed member LLRs cross the Wald upper bound or any member rolls
back locally. Returns the final reports."
  (splice:open-audit-log log-path)
  (let ((spec (currency-patch-spec))
        (members (mapcar (lambda (m)
                           (destructuring-bind (host port &key fault) m
                             (let ((ms (connect-member host port)))
                               (setf (getf (member-state-final ms) :fault) fault)
                               ms)))
                         members-spec))
        (mailbox (make-mailbox)))
    (prepare-members members members-spec spec)
    (commit-members members)
    (start-member-readers members mailbox)
    (await-cluster-verdict members mailbox (log (/ (- 1d0 beta) alpha)))
    (let ((finals (collect-final-reports members)))
      (splice:audit :cluster-final :reports finals)
      (splice:close-audit-log)
      finals)))

;;; Messages from the reader threads, drained by the aggregation loop.
(defstruct mailbox (messages '()) (lock (sb-thread:make-mutex)))

(defun prepare-members (members members-spec spec)
  "PREPARE on every member; any veto aborts the cluster."
  (splice:audit :cluster-prepare :members (mapcar #'second members-spec)
                                 :spec-hash (getf spec :hash))
  (loop for ms in members
        for (nil nil . opts) in members-spec
        do (send-form (member-state-stream ms)
                      (list :prepare :spec spec :fault (getf opts :fault)))
           (let ((reply (read-form (member-state-stream ms))))
             (unless (eq (first reply) :prepared)
               (splice:audit :cluster-abort :reason reply)
               (error "member ~a vetoed prepare: ~a"
                      (member-state-port ms) reply)))))

(defun commit-members (members)
  "COMMIT everywhere, starting each member's local canary."
  (dolist (ms members)
    (send-form (member-state-stream ms) '(:commit))
    (let ((reply (read-form (member-state-stream ms))))
      (unless (eq (first reply) :committed)
        (error "member ~a failed commit: ~a" (member-state-port ms) reply))))
  (splice:audit :cluster-commit :members (mapcar #'member-state-port members)))

(defun start-member-readers (members mailbox)
  "One thread per member, posting (member . message) to MAILBOX until that
member reports a terminal state or its stream closes."
  (dolist (ms members)
    (setf (member-state-thread ms)
          (sb-thread:make-thread
           (lambda ()
             (loop for msg = (ignore-errors (read-form (member-state-stream ms)))
                   until (or (null msg) (eq msg :eof))
                   do (sb-thread:with-mutex ((mailbox-lock mailbox))
                        (push (cons ms msg) (mailbox-messages mailbox)))
                   until (eq (first msg) :terminal)))
           :name (format nil "conductor-reader-~a" (member-state-port ms))))))

(defun await-cluster-verdict (members mailbox upper)
  "Fold member messages into MEMBERS until every member is terminal.
Broadcasts rollback once, on a local reject or when the pooled LLR crosses
UPPER."
  (let ((rollback-sent nil))
    (loop
      (sleep 0.05)
      (let ((msgs (sb-thread:with-mutex ((mailbox-lock mailbox))
                    (prog1 (reverse (mailbox-messages mailbox))
                      (setf (mailbox-messages mailbox) '())))))
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
              (local-rolling (find-if (lambda (state)
                                        (member state '(:rolling-back :restoring)))
                                      members :key #'member-state-state)))
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
          (return))))))

(defun collect-final-reports (members)
  "Ask each member for its final report and close the connection. A member
whose stream ended reports :state :died."
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
          members))
