;;;; view.lisp: The CLOG page for the demo.

(defpackage #:splice-orders/demo-clog
  (:use #:common-lisp)
  (:local-nicknames (#:d #:splice-orders/demo))
  (:export #:start-view #:stop-view #:write-frame
           #:show-chain-check #:show-tamper-check
           #:switch-to-replay #:start-live-act
           #:restart-live-view))

(in-package #:splice-orders/demo-clog)

(defvar *connections* '()
  "Open pages as (body . last-snapshot), newest first")

(defvar *connections-lock* (sb-thread:make-mutex :name "demo-connections"))

;;; ---------------------------------------------------------------------
;;; Page

(defun on-new-window (body)
  "Build one tab's page, put it into the registry, and drop it when the tab closes."
  (build-page body)
  (register-connection body)
  (clog:run body)
  (forget-connection body))

(defun build-page (body)
  "Lay out BODY and wire its buttons. Runs on the tab's connection thread, before registering"
  (clog:create-div body :content (d:page-html))
  (clog:js-execute body (d:page-script))
  (wire-buttons body))

(defun wire-buttons (body)
  (on-click body "next" (lambda () (d:open-gate d:*gate* :next-stage)))
  (on-click body "release" (lambda () (d:open-gate d:*gate* :release-held))))

(defun on-click (body id action)
  "Call ACTION/0 when the element ID in BODY is clicked"
  (clog:set-on-click (clog:attach-as-child body id)
                     (lambda (element)
                       (declare (ignore element))
                       (funcall action))))

;;; ---------------------------------------------------------------------
;;; Connection registry

(defun register-connection (body)
  (sb-thread:with-mutex (*connections-lock*)
    (push (cons body nil) *connections*)))

(defun forget-connection (body)
  (sb-thread:with-mutex (*connections-lock*)
    (setf *connections* (remove body *connections* :key #'car))))

(defun live-connections ()
  "Copy of the registry, safe to walk outside the lock!"
  (sb-thread:with-mutex (*connections-lock*)
    (copy-alist *connections*)))

(defun remember-snapshot (body snapshot)
  "Record SNAPSHOT as what BODY now shows. NIL means next frame is a full one"
  (sb-thread:with-mutex (*connections-lock*)
    (setf *connections*
          (mapcar (lambda (entry) (if (eq (car entry) body) (cons body snapshot) entry))
                  *connections*))))

(defun forget-snapshots ()
  "Make every page's next frame a full one. Player calls this after a jump"
  (sb-thread:with-mutex (*connections-lock*)
    (setf *connections* (mapcar (lambda (entry) (cons (car entry) nil)) *connections*))))

;;; ---------------------------------------------------------------------
;;; Frames

(defun send-frame (body scripts)
  "Send SCRIPTS to BODY as a websocket message"
  (clog:with-connection-cache (body)
    (dolist (script scripts)
      (clog:js-execute body script))))

(defun deliver (body scripts snapshot)
  "Send SCRIPTS to BODY and remember SNAPSHOT as what it shows.
:sent or :failed"
  (handler-case (progn (send-frame body scripts)
                       (remember-snapshot body snapshot)
                       :sent)
    (error ()
      (remember-snapshot body nil)
      :failed)))

(defun write-frame (snapshot)
  "The sink: bring every open page up to SNAPSHOT. Return how many pages took it"
  (count :sent (mapcar (lambda (entry)
                         (deliver (car entry) (d:render-frame (cdr entry) snapshot) snapshot))
                       (live-connections))))

;;; ---------------------------------------------------------------------
;;; Server

(defun start-view (&key (port 8417))
  "Serve demo on 127.0.0.1:PORT"
  (setf clog-connection:*break-on-error* nil)
  (clog:initialize #'on-new-window :host "127.0.0.1" :port port))

(defun stop-view ()
  (clog:shutdown))
