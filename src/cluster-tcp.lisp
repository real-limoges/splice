;;;; cluster-tcp.lisp: cluster protocol over loopback TCP
;;;;
;;;; One printed list per line. Member serves one condutor connection
;;;; and drives its host's workload once commited. Connductor
;;;; connects to every member, starts a reader thread per member
;;;; that hands messages to CONDUCTOR-RECEIVE, ships the patch
;;;; and waits for the cluster to finish.

(in-package #:splice/cluster)


(defvar *wire-lock* (sb-thread:make-mutex)
  "Serializes writes to every cluster stream in the image.
Replies, health reports, and the watch thread's message never interleave")

(defun send-form (stream form)
  (sb-thread:with-mutex (*wire-lock*)
    (let ((*package* (find-package :splice/cluster))
          (*print-pretty* nil))
      (prin1 form stream)
      (terpri stream)
      (force-output stream))))

(defun read-form (stream)
  "Next message on STREAM (or :EOF). *READ-EVAL* is off so a peer's
text never runs here"
  (let ((*package* (find-package :splice/cluster))
        (*read-eval* nil))
    (read stream nil :eof)))

;;; Member over TCP

(defun audit-log-report (log-path)
  "TCP member's own :final fields. The chain and reconstruction checks
of its closed audit log"
  (multiple-value-bind (ok entry-count) (splice:verify-audit-chain log-path)
    (list :chain-ok ok
          :entries entry-count
          :reconstruction-ok (and (splice:verify-reconstruction log-path) t))))

(defstruct (tcp-member-adapter
            (:constructor make-tcp-member-adapter (stream log-path host)))
  (stream nil :read-only t)
  (log-path nil :read-only t)
  (host nil :read-only t))

(defmethod send-to-conductor ((adapter tcp-member-adapter) message)
  (send-form (tcp-member-adapter-stream adapter) message))

(defmethod member-committed ((adapter tcp-member-adapter) cluster-member ps)
  "Hand commit to host. It starts its workload if it has one"
  (member-committed (tcp-member-adapter-host adapter) cluster-member ps))

(defmethod final-report-fields ((adapter tcp-member-adapter) ps)
  "Host's fields and this member's log checks. Host first so its workload is
done before the log closes"
  (let ((host-fields (final-report-fields (tcp-member-adapter-host adapter) ps)))
    (splice:close-audit-log)
    (append (audit-log-report (tcp-member-adapter-log-path adapter))
            host-fields)))

(defun run-member (&key (port 7801)
                     (log-path (format nil "/tmp/splice-member-~a.log" port))
                     host)
  "Serve a conductor connection on loopback PORT with the cluster protocol.
HOST (if given) gets MEMBER-COMMITTED and FINAL-REPORT-FIELDS"
  (let ((server (make-instance 'sb-bsd-sockets:inet-socket
                               :type :stream :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address server) t)
    (sb-bsd-sockets:socket-bind server #(127 0 0 1) port)
    (sb-bsd-sockets:socket-listen server 1)
    (splice:open-audit-log log-path)
    (let* ((stream (sb-bsd-sockets:socket-make-stream
                    (sb-bsd-sockets:socket-accept server) :input t :output t))
           (cluster-member (make-member
                            :adapter (make-tcp-member-adapter stream log-path host))))
      (loop (let ((message (read-form stream)))
              (when (or (eq message :eof)
                        (eq :bye (member-receive cluster-member message)))
                (return))))
      (ignore-errors (close stream))
      (sb-bsd-sockets:socket-close server))))

;;; Conductor over TCP

(defun connect-member (port &key (retries 40))
  "A stream to the member on loopback PORT, retrying while it starts."
  (loop for attempt from 1 do
    (handler-case
        (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                                     :type :stream :protocol :tcp)))
          (sb-bsd-sockets:socket-connect socket #(127 0 0 1) port)
          (return (sb-bsd-sockets:socket-make-stream socket :input t :output t)))
      (sb-bsd-sockets:socket-error (e)
        (when (>= attempt retries) (error e))
        (sleep 0.25)))))

(defstruct (tcp-conductor-adapter
            (:constructor make-tcp-conductor-adapter (streams)))
  (streams nil :read-only t))           ; ((port . stream) ...)

(defmethod send-to-member ((adapter tcp-conductor-adapter) port message)
  "Write MESSAGE to member at PORT. Failed writes are dropped. Member's reader
sends (:GONE) once its stream ends"
  (ignore-errors
   (send-form (cdr (assoc port (tcp-conductor-adapter-streams adapter)))
              message)))

(defun start-conductor-reader (conductor port stream)
  "Thread handing every message from PORT to CONDUCTOR, then (:GONE)
once the stream ends"
  (sb-thread:make-thread
   (lambda ()
     (loop for message = (ignore-errors (read-form stream))
           until (or (null message) (eq message :eof))
           do (conductor-receive conductor port message))
     (conductor-receive conductor port '(:gone)))
   :name (format nil "conductor-reader-~a" port)))

(defun wait-for-phase (conductor phases)
  "Block until CONDUCTOR reaches one of PHASES and return it"
  (loop for phase = (conductor-phase conductor)
        until (member phase phases)
        do (sleep 0.05)
        finally (return phase)))

(defun run-conductor (spec members-spec &key (alpha 0.01d0) (beta 0.05d0)
                                          (log-path "/tmp/splice-conductor.log"))
  "MEMBERS-SPEC is a list of (host port). Ship SPEC to every member over TCP to run
the cluster to the end. Returns one final report per member, which is a plist headed :PORT.
Signals an error if a member vetoed"
  (splice:open-audit-log log-path)
  (let* ((streams (mapcar (lambda (member-spec)
                            (destructuring-bind (host port) member-spec
                              (declare (ignore host))
                              (cons port (connect-member port))))
                          members-spec))
         (conductor (make-conductor
                     :alpha alpha
                     :beta beta
                     :adapter (make-tcp-conductor-adapter streams))))
    (dolist (entry streams)
      (conductor-add-member conductor (car entry)))
    (let ((readers (mapcar (lambda (entry)
                             (start-conductor-reader conductor (car entry) (cdr entry)))
                           streams)))
      (conductor-ship conductor spec)
      (let ((phase (wait-for-phase conductor '(:done :aborted))))
        (dolist (reader readers)
          (sb-thread:join-thread reader :default nil :timeout 10))
        (dolist (entry streams)
          (ignore-errors (close (cdr entry))))
        (splice:close-audit-log)
        (when (eq phase :aborted)
          (error "a member refused the cluster patch. see ~a" log-path))
        (mapcar (lambda (final)
                  (list* :port (getf final :member) (cddr final)))
                (conductor-finals conductor))))))
