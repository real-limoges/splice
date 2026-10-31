(:version 1
 :seq 16
 :elapsed-ms 1765
 :audit-seq 241
 :paused-at 1
 :session (:act 1 :seed 1001 :fault nil :threads 3 :started 61204417 :audit-start-seq 0)
 :set (:name orders::currency-migration :state :canary-25 :fraction 25
       :stage-index 1 :ramp (5 25 100) :pinned-old 1 :pinned-new 0)
 :functions ((:name orders:price-order :installed :split)
             (:name orders:apply-discount :installed :split)
             (:name orders:total-invoice :installed :split))
 :class (:name orders:order :shape :union :phase :forward)
 :orders #(:migrated :pending :migrated :migrated :pending :migrated :pending :migrated :migrated :pending :migrated :pending)
 :canary-born (:old 1978 :new 139)
 :threads ((:name "orders-straggler" :pins ((orders::currency-migration . :old)))
           (:name "orders-worker-0" :pins ())
           (:name "orders-worker-1" :pins ())
           (:name "orders-worker-2" :pins ()))
 :sprt (:stage 1 :trimmed nil :upper 4.553877 :lower -2.985682 :min-n 50
        :errors ((1 . -0.020223137))
        :slow ((1 . -0.111225635)))
 :faults ())
