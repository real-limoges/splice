;;;; router.lisp: the deterministic draw, route pinning, and the lifecycle
;;;; state predicates the router leans on (types.lisp).

(in-package #:splice-tests)

(def-suite router :in splice-tests)
(in-suite router)

;;; State predicates

(test stage-keyword-names-the-stage
  (is (eq :canary-5 (splice::stage-keyword 5)))
  (is (eq :canary-100 (splice::stage-keyword 100))))

(test canary-stage-p-accepts-any-fraction
  (is-true (splice::canary-stage-p :canary-5))
  (is-true (splice::canary-stage-p :canary-37))
  (is-false (splice::canary-stage-p :canary-))
  (is-false (splice::canary-stage-p :canary))
  (is-false (splice::canary-stage-p :staged))
  (is-false (splice::canary-stage-p :promoting)))

(test finished-p
  (dolist (state '(:rolled-back :promoted :vetoed))
    (is-true (splice::finished-p (splice::make-patch-set :state state))))
  (dolist (state '(:registered :staged :canary-5 :promoting :rolling-back :frozen))
    (is-false (splice::finished-p (splice::make-patch-set :state state)))))

;;; The draw

(test draw-is-the-hash-prefix-mod-100
  (is (= (mod (parse-integer (subseq (sha256-hex "42|SOME-SET") 0 8) :radix 16)
              100)
         (splice::routing-draw 42 'some-set))))

(test draw-is-deterministic-and-in-range
  (loop for task from 1 to 500
        for draw = (splice::routing-draw task 'some-set)
        do (is (= draw (splice::routing-draw task 'some-set)))
           (is (and (integerp draw) (<= 0 draw 99)))))

(test draws-are-roughly-uniform
  ;; 4000 draws against a 25% fraction land near 1000; a 5 sigma band
  (let ((under (loop for task from 1 to 4000
                     count (< (splice::routing-draw task 'uniformity) 25))))
    (is (< 865 under 1135) "~a of 4000 draws fell under 25" under)))

(test draws-differ-between-sets
  (is-false (every (lambda (task)
                     (= (splice::routing-draw task 'set-a)
                        (splice::routing-draw task 'set-b)))
                   (loop for task from 1 to 50 collect task))))

;;; Pinning

(defun set-in (state &key (fraction 0) (name 'pin-probe))
  (splice::make-patch-set :name name :state state :current-fraction fraction))

(test pin-follows-the-draw-in-a-canary-stage
  (let ((ps (set-in :canary-25 :fraction 25)))
    (loop for task from 1 to 200
          do (is (eq (if (< (splice::routing-draw task 'pin-probe) 25) :new :old)
                     (splice::pin-route ps task))))))

(test pin-at-full-fraction-is-always-new
  (let ((ps (set-in :canary-100 :fraction 100)))
    (is (every (lambda (task) (eq :new (splice::pin-route ps task)))
               (loop for task from 1 to 200 collect task)))))

(test promoting-pins-new-and-everything-else-pins-old
  (is (eq :new (splice::pin-route (set-in :promoting :fraction 0) 1)))
  ;; a fraction left over from the canary must not leak into other states
  (dolist (state '(:registered :staged :rolling-back :frozen :rolled-back :promoted))
    (is (eq :old (splice::pin-route (set-in state :fraction 100) 1))
        "~s pinned :new" state)))

(test task-route-for-reads-the-pin
  (let ((ps (set-in :canary-5)))
    (is (eq :old (task-route-for ps)))
    (let ((splice::*task-epochs* (list (list ps :new))))
      (is (eq :new (task-route-for ps))))))

(test name-of-tolerates-a-missing-set
  (is (string= "<none>" (splice::name-of nil)))
  (is (eq 'pin-probe (splice::name-of (set-in :staged)))))
