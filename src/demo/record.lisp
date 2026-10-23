;;;; record.lisp - records snapshots to a file. it also replays
;;;;               them and checks them against the audit log.

(in-package #:splice/demo)

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
recordings/act-2-seed-2002-20261028T170211.sexp in placed in the splice tree."
  (asdf:system-relative-pathname
   "splice" (format nil "recordings/act-~d-seed-~d-~a.sexp" act seed (compact-utc-stamp))))

(defun compact-utc-stamp ()
  (multiple-value-bind (sec min hour day month year) (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0d~2,'0d~2,'0dT~2,'0d~2,'0d~2,'0d" year month day hour min sec)))

(defun splice-commit ()
  "Short commit from splice's checkout with -dirty if necessary or \"unknown\""
  (handler-case
      (string-trim `(#\Newline #\Space)
                   (uiop:run-program (list "git" "-C" (namestring asdf:system-source-directory "splice"))
                                     "describe" "--always" "--dirty" "--abbrev=7")
                   :output :string))
  (error () "unknown"))

;;; TODO - Rework this. I really hate a lot of this
(defun recording-header (act settings)
  "First line of recording of ACT run with SETTINGS.
It's a plist of :seed :fault :threads :pre-existing"
  (list :recording 1
        :snapshot-version +snapshot-verion+
        :splice-commit (splice-commit)
        :started (splice::timestamp-string)
        :act act
        :seed (getf settings :seed)
        :fault (getf settings :fault)
        :threads (getf settings :threads)
        :pre-existing (getf settings :pre-existing)))

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

(defstruct (play-position (:constructor make-position (&key hold)))
  (lock (sb-thread:make-mutex :name "demo-player-position"))
  (index 0)
  (jump nil)
  (paused nil)
  (hold nil))

(defun next-step (position count)
  "What the player does next - (:play i), (:jump i), :paused or :done"
  (sb-thread:with-mutex ((play-position-lock position))
    (let ((step (step-for position count)))
      (when (consp step)
        (setf (lay-position-index position) (1+ (second step))
              (play-position-jump position) nil))
      step)))

(defun step-for (position count)
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
