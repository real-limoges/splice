;;;; snapshot.lisp

(in-package #:splice-orders/demo)

(defconstant +snapshot-version+ 1
  "Changes whenever a snapshot field changes meaning or shape")

(defparameter *snapshot-fields*
  '(:version :seq :elapsed-ms :audit-seq :paused-at :session :set :functions
    :class :orders :canary-born :threads :sprt :faults :replay)
  "Every top-level field of a version 1 snapshot, in printed order, then
:REPLAY, which only the player adds and nothing records.")

(defvar *current-act* nil
  "The act on screen for the snapshot's :session. Set by the act driver")

(defun snapshot-get (snapshot field)
  "FIELD of SNAPSHOT. It signals on the field the contract doesn't have so it
fails loudly instead of reading NIL"
  (assert (member field *snapshot-fields*) (field)
          "~s is not a snapshot field" field)
  (getf snapshot field))

;;; SPRT walk

(defun make-walk (stage trimmed bounds errors slow)
  (list :stage stage
        :trimmed trimmed
        :upper (getf bounds :upper)
        :lower (getf bounds :lower)
        :min-n (getf bounds :min-n)
        :errors errors
        :slow slow))

(defun start-walk (stage)
  "An empty walk for STAGE. Bounds are filled in when the frame finishes"
  (make-walk stage nil '() '() '()))

(defun add-point (points point)
  "POINTS with POINT, which is a (n . llr) pair. Unchanged when POINT is 0
   (the stream hasn't started or already drawn"
  (if (or (zerop (car point))
          (eql (car point) (car (first (last points)))))
      points
      (append points (list point))))

(defun extend-walk (walk step)
  "WALK with the two points one :SPRT-STEP entry"
  (make-walk (getf walk :stage) (getf walk :trimmed) '()
             (add-point (getf walk :errors) (getf step :errors))
             (add-point (getf walk :slow) (getf step :slow))))

(defun fold-step (walk step)
  "Extend WALK with STEP (or start a fresh walk when STEP is from another stage)"
  (extend-walk (if (eql (getf step :stage) (getf walk :stage))
                   walk
                   (start-walk (Getf step :stage)))
               step))

(defun finish-walk (walk bounds tail)
  "WALK with this frame's BOUNDS. Mark trimmed if TAIL says teh audit tail no longer
   reached back to the cursor"
  (make-walk (getf walk :stage)
             (or (getf walk :trimmed) (eq tail :trimmed))
             bounds
             (getf walk :errors)
             (getf walk :slow)))

(defun next-sprt (walk readings)
  "WALK advanced by the frame's :SPRT-STEP. Each step's :STAGE decides whether it
   extends the walk or starts a new one."
  (finish-walk (reduce #'fold-step (getf readings :sprt-steps) :initial-value walk)
               (getf readings :bounds)
               (getf readings :tail)))

;;; Snapshots

(defun empty-snapshot ()
  "Session's first frame folds from. Empty stage 0 walk"
  (list :version +snapshot-version+ :seq -1 :audit-seq 0 :session '()
        :sprt (start-walk 0)))

(defun snapshot-for-session (snapshot started)
  "SNAPSHOT if it belongs to the sesion published at STARTED. Otherwsie a fresh
   empty snapshot, so one session's walk never runs into the next one's"
  (if (eql started (getf (getf snapshot :session) :started))
      snapshot
      (empty-snapshot)))

(defun next-snapshot (previous readings)
  "Snapshot after PREVIOUS, given READINGS taken from the image"
  (list :version +snapshot-version+
        :seq (1+ (getf previous :seq))
        :elapsed-ms (getf readings :elapsed-ms)
        :audit-seq (getf readings :audit-seq)
        :paused-at (getf readings :paused-at)
        :session (getf readings :session)
        :set (getf readings :set)
        :functions (getf readings :functions)
        :class (getf readings :class)
        :orders (getf readings :orders)
        :canary-born (getf readings :canary-born)
        :threads (getf readings :threads)
        :sprt (next-sprt (getf previous :sprt) readings)
        :faults (getf readings :faults)))

;;; Text form: One snapshot per line

(defun write-snapshot (form stream)
  "Print FORM readably on the one line of STREAM"
  (with-standard-io-syntax
    (let ((*package* (find-package '#:splice-orders/demo))
          (*print-readably* t)
          (*print-pretty* nil))
      (prin1 form stream)
      (terpri stream))))

(defun read-snapshot (stream)
  "Next form on STREAM. :EOF at the end or a line cut short"
  (with-standard-io-syntax
    (let ((*package* (find-package '#:splice-orders/demo))
          (*read-eval* nil))
      (handler-case (read stream nil :eof)
        (end-of-file () :eof)))))

(defun read-all-snapshots (stream)
  "Every form left on STREAM (in order)"
  (loop for form = (read-snapshot stream)
        until (eq form :eof)
        collect form))
