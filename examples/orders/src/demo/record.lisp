;;;; record.lisp - records snapshots to a file. it also replays
;;;;               them and checks them against the audit log.

(in-package #:splice-orders/demo)

(defstruct (play-position (:constructor make-position (&key hold)))
  (lock (sb-thread:make-mutex :name "demo-player-position"))
  (index 0)
  (jump nil)
  (paused nil)
  (hold nil))

(defstruct player thread position (running t))

(defvar *player* nil
  "Replay player: its thread, position, and stop flag. NIL before first replay")

;;; ---------------------------------------------------------------------
;;; Recording

(defun make-recorder (path header)
  "Open PATH and write the HEADER. Returns two values;
    - A sink that appends snapshots
    - Function with 0/arity that closes the file"
  (ensure-directories-exist path)
  (let ((stream (open path :direction :output :if-exists :supersede)))
    (write-snapshot header stream)
    (values (lambda (snapshot)
              (write-snapshot snapshot stream)
              (finish-output stream))
            (lambda () (close stream)))))

(defun recording-path (act seed)
  "Where a new recording of ACT goes. It looks like:
recordings/act-2-seed-2002-20261028T170211.sexp in placed in the example's tree."
  (asdf:system-relative-pathname
   "splice-orders" (format nil "recordings/act-~d-seed-~d-~a.sexp" act seed (compact-utc-stamp))))

(defun compact-utc-stamp ()
  (multiple-value-bind (sec min hour day month year) (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0d~2,'0d~2,'0dT~2,'0d~2,'0d~2,'0d" year month day hour min sec)))

(defun splice-commit ()
  "Short commit from splice's checkout with -dirty if necessary or \"unknown\""
  (handler-case
      (string-trim `(#\Newline #\Space)
                   (uiop:run-program (list "git" "-C" (namestring (asdf:system-source-directory "splice"))
                                           "describe" "--always" "--dirty" "--abbrev=7")
                                     :output :string))
    (error () "unknown")))

;;; TODO - Rework this. I really hate a lot of this
(defun recording-header (act settings)
  "First line of recording of ACT run with SETTINGS.
It's a plist of :seed :fault :threads :pre-existing"
  (list :recording 1
        :snapshot-version +snapshot-version+
        :splice-commit (splice-commit)
        :started (splice::timestamp-string)
        :act act
        :seed (getf settings :seed)
        :fault (getf settings :fault)
        :threads (getf settings :threads)
        :pre-existing (getf settings :pre-existing)))

;;; ---------------------------------------------------------------------
;;; Loading

(defun load-recording (path &key (commit (splice-commit)))
  "Read at PATH. Still working out what to return here"
  (with-open-file (in path)
    (let* ((header (read-snapshot in))
           (snapshots (coerce (read-all-snapshots in) 'vector)))
      (values (recording-status header snapshots commit) header snapshots))))

(defun recording-status (header snapshots commit)
  (cond ((not (and (consp header) (eql 1 (getf header :recording)))) :wrong-recording-version)
        ((zerop (length snapshots)) :empty)
        ((notevery #'current-version-p (cons header (coerce snapshots 'list)))
         :wrong-snapshot-version)
        ((and commit (not (equal commit (getf header :splice-commit)))) :wrong-commit)
        (t :loaded)))

(defun current-version-p (form)
  "True when FORM (a header or snapshot) is at this image's snapshot version."
  (eql +snapshot-version+ (or (getf form :snapshot-version) (getf form :version))))

;;; ---------------------------------------------------------------------
;;; Replay

(defun next-step (position count)
  "What the player does next - (:play i), (:jump i), :paused or :done"
  (sb-thread:with-mutex ((play-position-lock position))
    (let ((step (command-for position count)))
      (when (consp step)
        (setf (play-position-index position) (1+ (second step))
              (play-position-jump position) nil))
      step)))

(defun command-for (position count)
  (let ((jump (play-position-jump position))
        (index (play-position-index position)))
    (cond (jump (list :jump (max 0 (min jump (1- count)))))
          ((play-position-paused position) :paused)
          ((< index count) (list :play index))
          ((play-position-hold position) :paused)
          (t :done))))

(defun frame-delay (snapshots index speed)
  "Seconds frmo snapshot INDEX to the next at SPEED.
0 at speed 0 or after the last frame."
  (if (or (zerop speed) (>= (1+ index) (length snapshots)))
      0
      (max 0 (/ (- (getf (aref snapshots (1+ index)) :elapsed-ms)
                   (getf (aref snapshots index) :elapsed-ms))
                1000 speed))))

(defun play-step (step snapshots sink speed on-jump)
  "Carry out one STEP from next-ste, wait while paused or send one frame and sleep until the next"
  (if (eq step :paused)
      (sleep 0.05)
      (destructuring-bind (kind index) step
        (when (eq kind :jump) (funcall on-jump))
        (funcall sink (aref snapshots index) index)
        (sleep (frame-delay snapshots index speed)))))

(defun replay-recording (snapshots sink &key (speed 1) (position (make-position))
                                          (on-jump (constantly :ignore))
                                          (running (constantly t)))
  "Feed SNAPSHOTS to SINK. Honors POSITION's jumps and pauses/calls ON-JUMP before jumping-to frames.
   Returns :finished or :stopped"
  (loop
    (unless (funcall running) (return :stopped))
    (let ((step (next-step position (length snapshots))))
      (when (eq step :done) (return :finished))
      (play-step step snapshots sink speed on-jump))))

;;; ---------------------------------------------------------------------
;;; Player

(defun mark-replay (snapshot header index count faults)
  "SNAPSHOT with :replay plist for the replay row, and the player's FAULTS in
of any it was recorded with. Never recorded."
  (list* :replay (list :started (getf header :started) :index index :frames count)
         :faults (append faults (getf snapshot :faults))
         snapshot))

(defun player-alive-p ()
  (and *player* (sb-thread:thread-alive-p (player-thread *player*))))

(defun start-player (header snapshots sink &key (reset (constantly :ignored)) (speed 1))
  "Play SNAPSHOTS into SINK. RESET is called after ever jump.
Returns :started or :already-running"
  (if (player-alive-p)
      :already-running
      (let ((player (make-player :position (make-position :hold t))))
        (setf (player-thread player)
              (sb-thread:make-thread (lambda () (run-player player header snapshots sink reset speed))
                                     :name "demo-player"))
        (setf *player* player)
        :started)))

(defun run-player (player header snapshots sink reset speed)
  (let ((faults '()))
    (replay-recording snapshots
                      (lambda (snapshot index)
                        (setf faults (call-player-sink
                                      sink (mark-replay snapshot header index (length snapshots) faults))))
                      :speed speed
                      :position (player-position player)
                      :on-jump reset
                      :running (lambda () (player-running player)))))

(defun call-player-sink (sink frame)
  "Call SINK on FRAME. Returns faults for next frame: () or a list holding the error string"
  (handler-case (progn (funcall sink frame) '())
    (error (e) (list (fault-string "replay sink failed" e)))))

(defun stop-player ()
  "Stop player + wait for thread to exit.
Returns :stopped or :not-running"
  (if (player-alive-p)
      (progn  (setf (player-running *player*) nil)
              (sb-thread:join-thread (player-thread *player*) :default :aborted)
              :stopped)
      :not-running))

(defun jump-player (index)
  "Ask player to show frame INDEX.
Returns :jumping, :ignored, or :no-player"
  (cond ((not (player-alive-p)) :no-player)
        ((not (integerp index)) :ignored)
        (t (let ((position (player-position *player*)))
             (sb-thread:with-mutex ((play-position-lock position))
               (setf (play-position-jump position) index)))
           :jumping)))

(defun pause-player (paused)
  "Hold when PAUSED is true, play when false.
Returns :paused, :playing, or :no-player"
  (if (player-alive-p)
      (let ((position (player-position *player*)))
        (sb-thread:with-mutex ((play-position-lock position))
          (setf (play-position-paused position) paused))
        (if paused :paused :playing))
      :no-player))
