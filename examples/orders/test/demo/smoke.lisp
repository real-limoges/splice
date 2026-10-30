;;;; smoke.lisp: an opt-in check that the CLOG view serves its page

(in-package #:splice-orders/demo-tests)

(def-suite demo-smoke :description "Starts CLOG; loaded only by splice-orders/demo-clog-tests.")
(in-suite demo-smoke)

(defun connect-when-listening (port &key (tries 50))
  "A socket connected to 127.0.0.1:PORT, retrying every 0.1 s while nothing listens yet."
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (handler-case (progn (sb-bsd-sockets:socket-connect socket #(127 0 0 1) port)
                         socket)
      (sb-bsd-sockets:connection-refused-error (condition)
        (sb-bsd-sockets:socket-close socket)
        (when (<= tries 1)
          (error condition))
        (sleep 0.1)
        (connect-when-listening port :tries (1- tries))))))

(defun http-get (port)
  "The whole response to GET / on 127.0.0.1:PORT, status line first."
  (let ((socket (connect-when-listening port)))
    (let ((stream (sb-bsd-sockets:socket-make-stream socket :input t :output t
                                                            :external-format :latin-1)))
      (unwind-protect
           (progn (format stream "GET / HTTP/1.0~c~c~c~c" #\Return #\Newline #\Return #\Newline)
                  (finish-output stream)
                  (with-output-to-string (out)
                    (loop for line = (read-line stream nil)
                          while line
                          do (write-line line out))))
        (close stream)))))

(test view-serves-page
  (splice-orders/demo-clog:start-view :port 8418)
  (unwind-protect
       (let ((response (http-get 8418)))
         (is (search "200" response :end2 (position #\Newline response)))
         (is (search "/js/boot.js" response)))
    (splice-orders/demo-clog:stop-view)))
