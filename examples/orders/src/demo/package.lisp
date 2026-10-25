;;;; package.lisp: the package for the live-image demo's browser-free code.

(defpackage #:splice-orders/demo
  (:use #:common-lisp)
  (:local-nicknames (#:orders #:splice-orders))
  (:export
   ;; the snapshot contract
   #:+snapshot-version+ #:*current-act*
   #:empty-snapshot #:next-snapshot #:snapshot-get
   #:write-snapshot #:read-snapshot #:read-all-snapshots

   ;; the sampler
   #:start-sampler #:stop-sampler #:run-sampler #:sample-once

   ;; renderers and the presenter gate
   #:render-frame #:chain-strip-svg #:js-string #:*palette*
   #:page-html #:page-script
   #:gate #:make-gate #:open-gate #:await-gate

   ;; the audit chain strip
   #:chain-result #:chain-check-js #:reconstruction-text #:tamper-copy

   ;; recording and replay
   #:make-recorder #:load-recording #:replay-recording
   #:start-player #:stop-player #:jump-player #:pause-player
   #:check-recording

   ;; the act driver
   #:*gate* #:*demo-log-path*
   #:start-act #:run-act #:reset-between-acts
   #:start-recording #:stop-recording #:restart-sampler))
