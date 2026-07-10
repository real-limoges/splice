;;;; audit.lisp — append-only, hash-chained provenance log (DESIGN.md §8).
;;;; One readably-printed plist per line; each entry carries the SHA-256 of
;;;; the previous entry's line text. The log alone reconstructs the image's
;;;; expected code state (RECONSTRUCT-STATE); LIFECYCLE.LISP provides the
;;;; live-image comparison half (VERIFY-RECONSTRUCTION).

(in-package #:splice)

(defvar *boot-id*
  (sha256-hex (format nil "~a-~a-~a" (get-universal-time)
                      (get-internal-real-time) (machine-instance)))
  "Identity of this image incarnation, stamped into every log entry.")

(defstruct (audit-log (:constructor %make-audit-log))
  path stream (seq 0) (prev-hash "genesis") (lock (sb-thread:make-mutex))
  ;; bounded in-memory tail (newest first) for cheap recent-entry queries;
  ;; the file is the source of truth
  (lines '()) (lines-count 0))

(defvar *audit-log* nil "The active audit log, or NIL for none.")

(defun open-audit-log (path &key (if-exists :supersede))
  (let ((stream (open path :direction :output :if-exists if-exists
                           :if-does-not-exist :create)))
    (setf *audit-log* (%make-audit-log :path path :stream stream))))

(defun close-audit-log ()
  (when *audit-log*
    (close (audit-log-stream *audit-log*))
    (setf *audit-log* nil)))

(defun timestamp-string ()
  (multiple-value-bind (sec min hour day month year) (get-decoded-time)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d.~3,'0d"
            year month day hour min sec
            (mod (round (/ (get-internal-real-time)
                           (/ internal-time-units-per-second 1000)))
                 1000))))

(defun sanitize-for-log (value)
  "Reduce VALUE to something READably printable. Structure instances,
functions, and other unREADable objects become (:opaque <hash-of-printed>)."
  (typecase value
    ((or number string keyword boolean) value)
    (symbol value)
    (cons (cons (sanitize-for-log (car value))
                (if (listp (cdr value))
                    (mapcar #'sanitize-for-log (cdr value))
                    (sanitize-for-log (cdr value)))))
    (condition (list :condition (string (type-of value))
                     (princ-to-string value)))
    (t (list :opaque (subseq (sha256-hex (princ-to-string value)) 0 16)))))

(defun audit (event &rest fields)
  "Append an entry to the audit log. EVENT is a keyword; FIELDS a plist.
Returns the entry's sequence number, or NIL when no log is open."
  (when *audit-log*
    (sb-thread:with-mutex ((audit-log-lock *audit-log*))
      (let* ((log *audit-log*)
             (entry (list* :seq (incf (audit-log-seq log))
                           :ts (timestamp-string)
                           :prev-hash (audit-log-prev-hash log)
                           :boot-id (subseq *boot-id* 0 12)
                           :event event
                           (mapcar #'sanitize-for-log fields)))
             (line (with-standard-io-syntax
                     (let ((*print-readably* nil) (*print-pretty* nil)
                           (*package* (find-package :splice)))
                       (prin1-to-string entry)))))
        (write-line line (audit-log-stream log))
        (finish-output (audit-log-stream log))
        (push line (audit-log-lines log))
        (when (> (incf (audit-log-lines-count log)) 6000)
          ;; keep the newest 3000; long batches must not grow the heap
          (setf (cdr (nthcdr 2999 (audit-log-lines log))) nil
                (audit-log-lines-count log) 3000))
        (setf (audit-log-prev-hash log) (sha256-hex line))
        (audit-log-seq log)))))

;;; ---------------------------------------------------------------------
;;; Offline: chain verification and state reconstruction

(defun map-audit-entries (path fn &key (verify t))
  "Stream the log at PATH through FN (called with each parsed entry),
verifying the hash chain along the way. Returns (values ok count
break-seq). Streaming: multi-hundred-thousand-entry logs must not be
resident all at once."
  (with-open-file (in path)
    (let ((*read-eval* nil)
          (*package* (find-package :splice))
          (expected "genesis")
          (count 0))
      (loop for line = (read-line in nil nil)
            while line
            do (let ((entry (read-from-string line)))
                 (when (and verify
                            (not (equal (getf entry :prev-hash) expected)))
                   (return-from map-audit-entries
                     (values nil count (getf entry :seq))))
                 (setf expected (sha256-hex line))
                 (incf count)
                 (when fn (funcall fn entry))))
      (values t count nil))))

(defun verify-audit-chain (path)
  "Verify the hash chain of the log at PATH. Returns (values ok entry-count
break-seq): OK is T when intact; BREAK-SEQ is the seq of the first entry
whose :prev-hash does not match, or NIL."
  (map-audit-entries path nil))

(defun reconstruct-state (path)
  "Fold over the audit log to derive the image's expected code state.
Returns a plist: (:functions alist(name . impl) :classes alist(name . shape)
:sets alist(name . final-state)) where IMPL is :original, :shimmed, or
(:new set-name), and SHAPE is :original, :union, or :new."
  (let ((functions '()) (classes '()) (sets '()) (members '()))
      (flet ((set-impl (name impl)
               (setf functions (acons name impl (remove name functions :key #'car))))
             (set-shape (name shape)
               (setf classes (acons name shape (remove name classes :key #'car))))
             (set-state (name state)
               (setf sets (acons name state (remove name sets :key #'car)))))
        (flet ((fold (entry)
          (let ((set-name (getf entry :patch-set)))
            (case (getf entry :event)
              (:patch-registered
               (setf members (acons set-name
                                    (list :functions (getf entry :functions)
                                          :classes (getf entry :classes))
                                    members))
               (set-state set-name :registered))
              (:contract-result
               (when (eq (getf entry :verdict) :veto) (set-state set-name :vetoed)))
              (:install
               (set-state set-name :staged)
               (dolist (f (getf (cdr (assoc set-name members)) :functions))
                 (set-impl (getf f :name) :shimmed))
               (dolist (c (getf (cdr (assoc set-name members)) :classes))
                 (set-shape c :union)))
              (:canary-transition (set-state set-name (getf entry :to)))
              (:promotion
               (set-state set-name :promoted)
               (dolist (f (getf (cdr (assoc set-name members)) :functions))
                 (set-impl (getf f :name) (list :new set-name)))
               (dolist (c (getf (cdr (assoc set-name members)) :classes))
                 (set-shape c :new)))
              (:rollback-complete
               (set-state set-name :rolled-back)
               (dolist (f (getf (cdr (assoc set-name members)) :functions))
                 (set-impl (getf f :name) :original))
               (dolist (c (getf (cdr (assoc set-name members)) :classes))
                 (set-shape c :original)))
              (:freeze (set-state set-name :frozen))))))
          (multiple-value-bind (ok count break-seq)
              (map-audit-entries path #'fold)
            (declare (ignore count))
            (unless ok
              (error "audit chain broken at seq ~a — refusing to reconstruct"
                     break-seq)))))
    (list :functions functions :classes classes :sets sets)))
