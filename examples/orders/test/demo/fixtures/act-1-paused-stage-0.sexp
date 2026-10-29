(:version 1
 :seq 0
 :elapsed-ms 99
 :audit-seq 9
 :paused-at 0
 :session (:act 1 :seed 1001 :fault nil :threads 3 :started 61204417 :audit-start-seq 0)
 :set (:name orders::currency-migration :state :canary-5 :fraction 5
       :stage-index 0 :ramp (5 25 100) :pinned-old 0 :pinned-new 0)
 :functions ((:name orders:price-order :installed :split)
             (:name orders:apply-discount :installed :split)
             (:name orders:total-invoice :installed :split))
 :class (:name orders:order :shape :union :phase :forward)
 :orders #(:pending :pending :pending :pending :pending :pending :pending :pending :pending :pending :pending :pending)
 :canary-born (:old 4 :new 0)
 :threads ((:name "orders-worker-0" :pins ())
           (:name "orders-worker-1" :pins ())
           (:name "orders-worker-2" :pins ()))
 :sprt (:stage 0 :trimmed nil :upper 4.553877 :lower -2.985682 :min-n 50
        :errors ()
        :slow ())
 :faults ())
