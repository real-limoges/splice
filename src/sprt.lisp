;;;; sprt.lisp: Wald sequential probability ratio test
;;;; One instance per (patch-set stage signal-stream).
;;;; H0: p = p0 (healthy, tracked from the control arm),
;;;; H1: p = p1 = max(c*p0, p0+delta).
;;;; "Accept H0" is Wald's term (beta bounds the false-accept rate) and
;;;; means "closer to p0 than to p1", not "p is p0": a true p between p0
;;;; and p1 can end at either boundary.
;;;; Thresholds carry Wald's alpha/beta bounds. Statistics reset at each ramp
;;;; stage. A lucky 5% stage must not subsidize a bad 25% stage.

(in-package #:splice)

;;; ---------------------------------------------------------------------
;;; Sequential test

(defstruct (sprt (:constructor %make-sprt))
  (llr 0.0d0 :type double-float)
  (n 0 :type fixnum)
  (p0 0.001d0 :type double-float)
  (p1 0.02d0 :type double-float)
  (upper 0.0d0 :type double-float)   ; A: cross above => reject H0 (failing)
  (lower 0.0d0 :type double-float)   ; B: cross below => accept H0 (healthy)
  (alpha 0.01d0) (beta 0.05d0)
  (min-n 50 :type fixnum)            ; observation floor (no verdict until this)
  (max-n 2000 :type fixnum)          ; stage budget; exceeded => :undecided
  (p1-factor 3.0d0) (p1-delta 0.02d0)
  (fixed-p0 nil))                    ; T = do not re-estimate from control

(defun make-sprt (&key (alpha 0.01d0) (beta 0.05d0) (p0 0.001d0)
                    (p1 nil) (min-n 50) (max-n 2000)
                    (p1-factor 3.0d0) (p1-delta 0.02d0) (fixed-p0 nil))
  (let ((s (%make-sprt :alpha alpha :beta beta :min-n min-n :max-n max-n
                       :p1-factor p1-factor :p1-delta p1-delta
                       :fixed-p0 fixed-p0
                       :upper (log (/ (- 1d0 beta) alpha))
                       :lower (log (/ beta (- 1d0 alpha))))))
    (sprt-set-p0 s p0)
    (when p1 (setf (sprt-p1 s) (coerce p1 'double-float)))
    s))

(defun sprt-set-p0 (sprt p0)
  "Update the healthy-hypothesis rate (from the control arm) and re-derive
p1. Clamped away from 0 and 1 so log-likelihoods stay finite."
  (let ((p0 (min 0.5d0 (max 1d-4 (coerce p0 'double-float)))))
    (setf (sprt-p0 sprt) p0
          (sprt-p1 sprt) (min 0.9d0 (max (* (sprt-p1-factor sprt) p0)
                                         (+ p0 (sprt-p1-delta sprt)))))
    sprt))

(defun sprt-observe (sprt failure-p)
  "Fold in a canary observation and return SPRT-VERDICT."
  (let ((p0 (sprt-p0 sprt)) (p1 (sprt-p1 sprt)))
    (incf (sprt-llr sprt)
          (if failure-p
              (log (/ p1 p0))
              (log (/ (- 1d0 p1) (- 1d0 p0)))))
    (incf (sprt-n sprt))
    (sprt-verdict sprt)))

(defun sprt-verdict (sprt)
  ":continue while sampling
   :reject (canary failing => rollback)
   :accept (stage healthy => advance ramp)
   :undecided (stage budget spent => a human decides)"
  (cond ((< (sprt-n sprt) (sprt-min-n sprt)) :continue)
        ((>= (sprt-llr sprt) (sprt-upper sprt)) :reject)
        ((<= (sprt-llr sprt) (sprt-lower sprt)) :accept)
        ((>= (sprt-n sprt) (sprt-max-n sprt)) :undecided)
        (t :continue)))

(defun sprt-snapshot (sprt stream-kind)
  "Plist of the full test state for the audit log. Every verdict must be
reproducible from the log alone, because I'm paranoid"
  (list :stream stream-kind :n (sprt-n sprt)
        :llr (float (sprt-llr sprt) 1.0)
        :a (float (sprt-upper sprt) 1.0) :b (float (sprt-lower sprt) 1.0)
        :p0 (float (sprt-p0 sprt) 1.0) :p1 (float (sprt-p1 sprt) 1.0)
        :alpha (sprt-alpha sprt) :beta (sprt-beta sprt)))

;;; ---------------------------------------------------------------------
;;; Control-arm tracking
;;; Ring buffers of recent control outcomes/latencies give the baseline and
;;; the confound check.

(defstruct (control-ring (:constructor %make-control-ring))
  (outcomes nil) (latencies nil) (size 500) (head 0) (count 0)
  (lock (sb-thread:make-mutex)))

(defun make-control-ring (&key (size 500))
  (%make-control-ring :size size
                      :outcomes (make-array size :initial-element nil)
                      :latencies (make-array size :initial-element 0)))

(defun control-record (ring failure-p latency-ms)
  (sb-thread:with-mutex ((control-ring-lock ring))
    (setf (aref (control-ring-outcomes ring)  (control-ring-head ring)) failure-p
          (aref (control-ring-latencies ring) (control-ring-head ring)) latency-ms
          (control-ring-head ring) (mod (1+ (control-ring-head ring))
                                        (control-ring-size ring)))
    (setf (control-ring-count ring)
          (min (control-ring-size ring) (1+ (control-ring-count ring))))))

(defun control-failure-rate (ring &key (min-samples 30))
  "Failure rate over the ring, or NIL if too few samples to trust."
  (sb-thread:with-mutex ((control-ring-lock ring))
    (let ((n (control-ring-count ring)))
      (when (>= n min-samples)
        (/ (loop for i from 0 below n
                 count (aref (control-ring-outcomes ring) i))
           (float n 1d0))))))

(defun control-latency-quantile (ring q &key (min-samples 30))
  (sb-thread:with-mutex ((control-ring-lock ring))
    (let ((n (control-ring-count ring)))
      (when (>= n min-samples)
        (let ((sorted (sort (subseq (control-ring-latencies ring) 0 n) #'<)))
          (aref sorted (min (1- n) (floor (* q n)))))))))
