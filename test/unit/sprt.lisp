;;;; sprt.lisp: Wald's SPRT and the control ring.

(in-package #:splice-tests)

(def-suite sprt :in splice-tests)
(in-suite sprt)

(defun close-to (a b &optional (tolerance 1d-9))
  (< (abs (- a b)) tolerance))

(defun observe-until-decided (sprt failure-p limit)
  "Feed SPRT the same observation until its verdict leaves :CONTINUE or
LIMIT observations go by. Returns the verdict."
  (loop repeat limit
        for verdict = (splice:sprt-observe sprt failure-p)
        unless (eq verdict :continue)
          return verdict
        finally (return :continue)))

;;; Construction

(test thresholds-follow-wald
  (let ((s (splice:make-sprt :alpha 0.01d0 :beta 0.05d0)))
    (is (close-to (log (/ 0.95d0 0.01d0)) (splice:sprt-upper s)))
    (is (close-to (log (/ 0.05d0 0.99d0)) (splice:sprt-lower s)))
    (is (> (splice:sprt-upper s) 0 (splice:sprt-lower s)))))

(test p1-is-the-larger-of-factor-and-delta
  ;; small p0: the additive delta wins
  (is (close-to 0.021d0 (splice::sprt-p1 (splice:make-sprt :p0 0.001d0))))
  ;; larger p0: the multiplicative factor wins
  (is (close-to 0.3d0 (splice::sprt-p1 (splice:make-sprt :p0 0.1d0)))))

(test explicit-p1-overrides-the-derived-one
  (is (close-to 0.05d0 (splice::sprt-p1 (splice:make-sprt :p0 0.001d0 :p1 0.05)))))

(test p0-is-clamped-so-logs-stay-finite
  (is (close-to 1d-4 (splice::sprt-p0 (splice:make-sprt :p0 0))))
  (is (close-to 0.5d0 (splice::sprt-p0 (splice:make-sprt :p0 0.9d0))))
  ;; p1 is capped too: 3 x 0.5 would be a probability above 1
  (is (close-to 0.9d0 (splice::sprt-p1 (splice:make-sprt :p0 0.9d0)))))

(test set-p0-rederives-p1
  (let ((s (splice:make-sprt :p0 0.001d0)))
    (splice::sprt-set-p0 s 0.1d0)
    (is (close-to 0.1d0 (splice::sprt-p0 s)))
    (is (close-to 0.3d0 (splice::sprt-p1 s)))))

;;; Observation

(test each-observation-adds-its-log-likelihood-ratio
  (let* ((s (splice:make-sprt :p0 0.001d0))
         (p0 (splice::sprt-p0 s))
         (p1 (splice::sprt-p1 s)))
    (splice:sprt-observe s t)
    (is (close-to (log (/ p1 p0)) (splice:sprt-llr s)))
    (splice:sprt-observe s nil)
    (is (close-to (+ (log (/ p1 p0)) (log (/ (- 1 p1) (- 1 p0))))
                  (splice:sprt-llr s)))
    (is (= 2 (splice:sprt-n s)))))

(test no-verdict-before-min-n
  (let ((s (splice:make-sprt :min-n 50)))
    ;; two failures already cross the upper bound
    (dotimes (i 49)
      (is (eq :continue (splice:sprt-observe s t))))
    (is (> (splice:sprt-llr s) (splice:sprt-upper s)))
    (is (eq :reject (splice:sprt-observe s t)))))

(test a-healthy-stream-accepts
  (let ((s (splice:make-sprt :min-n 50 :max-n 2000)))
    (is (eq :accept (observe-until-decided s nil 2000)))
    (is (<= (splice:sprt-llr s) (splice:sprt-lower s)))
    (is (<= 50 (splice:sprt-n s) 2000))))

(test a-failing-stream-rejects
  (let ((s (splice:make-sprt :min-n 50)))
    (is (eq :reject (observe-until-decided s t 2000)))
    (is (= 50 (splice:sprt-n s)))))

(test spent-budget-is-undecided
  (let ((s (splice:make-sprt :min-n 50 :max-n 200)))
    (setf (splice:sprt-n s) 200
          (splice:sprt-llr s) 0d0)
    (is (eq :undecided (splice:sprt-verdict s)))))

(test a-bound-beats-the-budget
  (let ((s (splice:make-sprt :min-n 50 :max-n 200)))
    (setf (splice:sprt-n s) 500
          (splice:sprt-llr s) (+ (splice:sprt-upper s) 1d0))
    (is (eq :reject (splice:sprt-verdict s)))))

(test snapshot-carries-the-whole-test-state
  (let ((snap (splice::sprt-snapshot (splice:make-sprt) :error)))
    (is (eq :error (getf snap :stream)))
    (dolist (key '(:n :llr :a :b :p0 :p1 :alpha :beta))
      (is-true (member key snap) "snapshot is missing ~s" key))))

;;; Control ring

(defun ring-with (outcomes &key (size 500) (latency 1))
  (let ((ring (splice::make-control-ring :size size)))
    (dolist (failure-p outcomes ring)
      (splice::control-record ring failure-p latency))))

(test failure-rate-needs-enough-samples
  (is (null (splice::control-failure-rate (ring-with (make-list 29)))))
  (is (= 0 (splice::control-failure-rate (ring-with (make-list 30))))))

(test failure-rate-counts-failures
  (let ((ring (ring-with (append (make-list 30 :initial-element t)
                                 (make-list 90)))))
    (is (close-to 0.25d0 (splice::control-failure-rate ring)))))

(test the-ring-forgets-its-oldest-outcomes
  (let ((ring (ring-with (append (make-list 40 :initial-element t)
                                 (make-list 40))
                         :size 40)))
    (is (= 40 (splice::control-ring-count ring)))
    (is (= 0 (splice::control-failure-rate ring)))))

(test latency-quantile
  (let ((ring (splice::make-control-ring :size 100)))
    ;; record 100 down to 1 so the ring has to sort them
    (loop for ms from 100 downto 1
          do (splice::control-record ring nil ms))
    (is (= 51 (splice::control-latency-quantile ring 0.5)))
    (is (= 100 (splice::control-latency-quantile ring 0.99)))
    (is (= 100 (splice::control-latency-quantile ring 1.0)))
    (is (= 1 (splice::control-latency-quantile ring 0.0)))))

(test latency-quantile-needs-enough-samples
  (is (null (splice::control-latency-quantile (ring-with (make-list 29)) 0.5))))
