
(in-package #:splice-orders/demo)

(defparameter *palette*
  '(:bg "#1f2233" :ink "#e8e6df" :muted "#7c7f93"
    :old "#e0a458" :new "#5fb3d9"
    :reject "#e5534b" :accept "#6cc070"
    :pending "#e8e6df" :migrated "#5fb3d9" :current "#a58fd8" :stranded "#e5534b")
  "Every color the page uses, by role. Injected into the page CSS as --role.")

(defun color (key)
  "A CSS reference to palette entry KEY."
  (format nil "var(--~(~a~))" key))

;;; ---------------------------------------------------------------------
;;; Page skeleton

(defun page-css ()
  (format nil ":root{~{--~(~a~):~a;~}}~a" *palette*
          "body{margin:0;background:var(--bg);color:var(--ink);font:24px/1.4 sans-serif}
[hidden]{display:none!important}
.strip,.replay{display:flex;gap:24px;align-items:center;padding:12px 24px}
.panels{display:grid;grid-template-columns:1fr 1fr;gap:16px;padding:16px}
.chain,.banner{display:none;padding:24px}
body[data-mode=chain] .panels,body[data-mode=banner] .panels{display:none}
body[data-mode=chain] .chain,body[data-mode=banner] .banner{display:block}
button{font-size:24px;padding:8px 24px}
.lane{display:flex;gap:8px;min-height:36px}
.chip{padding:2px 12px;border-radius:6px;color:var(--bg)}
.chip.old{background:var(--old)}.chip.new{background:var(--new)}
.faults{color:var(--bg);background:var(--reject);padding:2px 12px;border-radius:6px}"))

(defun page-html ()
  "The page skeleton. Frames fill it in; nothing in it depends on a snapshot."
  (format nil "<style>~a</style>~a" (page-css)
          "<div class='strip'><span id='act'>Between sessions</span><span id='state'></span>
<span id='fraction'></span><span id='canary-born'></span><span id='faults' class='faults' hidden></span>
<span id='controls'><button id='next' disabled onclick='this.disabled=true'>Next</button>
<button id='release' disabled onclick='this.disabled=true'>Release</button></span></div>
<div class='replay' id='replay' hidden><span id='replay-label'></span>
<input id='scrub' type='range' min='0' max='0' value='0'>
<button id='pause'>Pause</button><button id='play'>Play</button></div>
<div class='banner' id='banner'></div>
<div class='panels'><div id='call-graph'></div><canvas id='field' width='600' height='600'></canvas>
<div id='lanes'></div><div id='evidence'></div></div>
<div class='chain'><div id='chain-label'></div><div id='chain-strip'></div>
<div id='chain-detail'></div><div id='reconstruction'></div></div>"))

(defun page-script ()
  "Page-side helpers: spliceDot and spliceClear for the field, and spacebar as Next."
  "(function(){
var css=getComputedStyle(document.documentElement);
function color(n){return css.getPropertyValue('--'+n).trim();}
var cols=20,cell=28;
window.spliceClear=function(){var c=document.getElementById('field');
 c.getContext('2d').clearRect(0,0,c.width,c.height);};
window.spliceDot=function(i,style){
 var g=document.getElementById('field').getContext('2d');
 var x=(i%cols)*cell+4,y=Math.floor(i/cols)*cell+4,s=cell-8;
 g.clearRect(x-2,y-2,s+4,s+4);g.lineWidth=3;g.strokeStyle=g.fillStyle=color(style);
 if(style==='migrated'||style==='current'){g.fillRect(x,y,s,s);}else{g.strokeRect(x,y,s,s);}
 if(style==='stranded'){g.beginPath();g.moveTo(x,y);g.lineTo(x+s,y+s);
  g.moveTo(x+s,y);g.lineTo(x,y+s);g.stroke();}};
document.addEventListener('keydown',function(e){
 if(e.key===' '){e.preventDefault();document.getElementById('next').click();}});
})();")

;;; ---------------------------------------------------------------------
;;; Script builders

(defun js-string (string)
  "STRING as a double-quoted JavaScript literal, inert even inside a script element."
  (with-output-to-string (out)
    (write-char #\" out)
    (loop for (char next) on (coerce string 'list)
          do (write-string (js-escape char next) out))
    (write-char #\" out)))

(defun js-escape (char next)
  (case char
    (#\\ "\\\\")
    (#\" "\\\"")
    (#\Newline "\\n")
    (#\Return "\\r")
    (#\< (if (eql next #\/) "<\\" "<"))
    (t (string char))))

(defun js-bool (flag)
  (if flag "true" "false"))

(defun html-escape (text)
  "TEXT with the four characters that matter in markup replaced by entities."
  (with-output-to-string (out)
    (loop for char across (princ-to-string text)
          do (write-string (case char
                             (#\& "&amp;") (#\< "&lt;") (#\> "&gt;") (#\' "&#39;")
                             (t (string char)))
                           out))))

(defun set-property (id name value)
  "JavaScript assigning VALUE, already a JavaScript literal, to property NAME of element ID."
  (format nil "document.getElementById(~a)[~a]=~a;" (js-string id) (js-string name) value))

(defun set-text (id text)
  (set-property id "textContent" (js-string (princ-to-string text))))

(defun set-disabled (id flag)
  (set-property id "disabled" (js-bool flag)))

(defun set-hidden (id flag)
  (set-property id "hidden" (js-bool flag)))

(defun set-mode (mode)
  (format nil "document.body.dataset.mode=~a;" (js-string mode)))

;;; ---------------------------------------------------------------------
;;; Frames

(defun same-input-p (input previous next)
  "True when PREVIOUS exists and INPUT, a function of a snapshot, gives equal results for both."
  (and previous (equal (funcall input previous) (funcall input next))))

(defun render-frame (previous next)
  "JavaScript strings that bring a page showing PREVIOUS up to NEXT.
PREVIOUS is NIL for a fresh page, which gets every region."
  (if (eql +snapshot-version+ (getf next :version))
      (remove :unchanged
              (list (if previous :unchanged (set-mode "live"))
                    (render-strip previous next)
                    (render-call-graph previous next)
                    (render-field previous next)
                    (render-lanes previous next)
                    (render-evidence previous next)
                    (render-replay previous next)))
      (list (render-version-banner next))))

(defun render-version-banner (snapshot)
  (concatenate 'string
               (set-mode "banner")
               (set-text "banner" (format nil "Unknown snapshot version ~a: update the view or re-record"
                                          (getf snapshot :version)))))

;;; ---------------------------------------------------------------------
;;; Status strip

(defun next-enabled-p (snapshot)
  "True while the pacer holds a stage: :paused-at is a stage index, not :none."
  (integerp (getf snapshot :paused-at)))

(defun strip-input (snapshot)
  (list (getf (getf snapshot :session) :act)
        (getf snapshot :set)
        (getf snapshot :paused-at)
        (getf snapshot :faults)))

(defun render-strip (previous next)
  (if (same-input-p #'strip-input previous next)
      :unchanged
      (let ((set (getf next :set)))
        (concatenate 'string
                     (set-text "act" (act-title (getf (getf next :session) :act)))
                     (set-text "state" (state-text set))
                     (set-text "fraction" (format nil "~a% new" (or (getf set :fraction) 0)))
                     (set-text "faults" (faults-text (getf next :faults)))
                     (set-hidden "faults" (null (getf next :faults)))
                     (set-disabled "next" (not (next-enabled-p next)))
                     (set-disabled "release" (not (draining-p set)))))))

(defun faults-text (faults)
  "The badge text for FAULTS, the sampler's strings from the previous frame."
  (format nil "~d fault~:p: ~a" (length faults) (or (first faults) "")))

(defun act-title (act)
  (case act
    (1 "Act 1: a clean patch promotes")
    (2 "Act 2: the VIP fault rolls back")
    (t "Between sessions")))

(defun draining-p (set)
  (and (eq :promoting (getf set :state))
       (plusp (or (getf set :pinned-old) 0))))

(defun state-text (set)
  (if (draining-p set)
      (format nil "waiting for ~d :old task~:p to drain" (getf set :pinned-old))
      (format nil "~(~s~)" (getf set :state))))

;;; ---------------------------------------------------------------------
;;; Call graph

(defun call-graph-input (snapshot)
  (list (getf snapshot :functions)
        (getf snapshot :set) :fraction))

(defun render-call-graph (previous next)
  (if (same-input-p #'call-graph-input previous next)
      :unchanged
      (set-property "call-graph" "innerHTML" (js-string (call-graph-svg next)))))

(defun call-graph-svg (snapshot)
  (let ((fraction (or (getf (getf snapshot :set) :fraction) 0)))
    (svg 640 520
         (list* (svg-rect 220 20 200 50 :ink)
                (svg-text 240 55 "process-order" :bg)
                (loop for fn in (getf snapshot :functions)
                      for y from 150 by 120
                      append (function-node fn y fraction))))))

(defun function-node (fn y fraction)
  "The edges from process-order and FN's old and new bodies as SVG parts at height Y"
  (destructuring-bind (old-style new-style) (body-styles (getf fn :installed))
    (let ((split (eq :split (getf fn :installed))))
      (list (svg-line 320 70 170 y (split-width (- 100 fraction) split) :old)
            (svg-line 320 70 470 y (split-width fraction split) :new)
            (svg-rect 70 y 200 50 old-style)
            (svg-rect 370 y 200 50 new-style)
            (if (eq new-style :reject) (svg-line 370 (+ y 50) 570 y 4 :reject) "")
            (svg-text 70 (- y 8) (string-downcase (getf fn :name)) :ink)
            (if split (svg-text 290 (+ y 35) (format nil "~d / ~d" (- 100 fraction) fraction) :ink) "")))))

(defun body-styles (installed)
  "Palette keys for the old and new bodies by what the function's name resolves to"
  (ecase installed
    (:split '(:old :new))
    (:new '(:muted :new))
    (:old '(:old :reject))
    (:foreign '(:reject :reject))))

(defun split-width (share split)
  (if split (+ 3 (round share 10)) 3))

;;; ---------------------------------------------------------------------
;;; Order field

(defun render-field (previous next)
  (let ((script (format nil "~a~{~a~}~a"
                        (if (field-reset-p previous next) "spliceClear();" "")
                        (changed-dots (if (field-reset-p previous next) #() (field-styles previous))
                                      (field-styles next))
                        (canary-born-text previous next))))
    (if (string= script "") :unchanged script)))

(defun field-reset-p (previous next)
  "True when teh field must be cleared and drawn whole (fresh page or new order count)"
  (or (null previous)
      (/= (length (getf previous :orders)) (length (getf next :orders)))))

(defun field-styles (snapshot)
  (let ((promoted (eq :promoted (getf (getf snapshot :set) :state))))
    (map 'vector (lambda (state) (dot-style state promoted)) (getf snapshot :orders))))

(defun dot-style (state promoted)
  (if (and promoted (eq state :pending))
      "stranded"
      (string-downcase state)))

(defun changed-dots (old new)
  "One spliceDot call per position whose style differs between OLD and NEW"
  (loop for style across new
        for i from 0
        unless (and (< i (length old)) (equal style (aref old i)))
          collect (format nil "spliceDot(~d,~a);" i (js-string style))))

(defun canary-born-text (previous next)
  (if (same-input-p (lambda (s) (getf s :canary-born)) previous next)
      ""
      (let ((born (getf next :canary-born)))
        (set-text "canary-born" (format nil "canary-born: ~d old, ~d new"
                                        (or (getf born :old) 0) (or (getf born :new) 0))))))

;;; ---------------------------------------------------------------------
;;; Thread lanes

(defun render-lanes (previous next)
  (if (same-input-p (lambda (s) (getf s :threads)) previous next)
      :unchanged
      (set-property "lanes" "innerHTML"
                    (js-string (format nil "~{~a~}" (mapcar #'lane-html (getf next :threads)))))))

(defun lane-html (thread)
  (format nil "<div class='lane'><span>~a</span>~{~a~}</div>"
          (html-escape (getf thread :name))
          (mapcar #'pin-chip-html (getf thread :pins))))

(defun pin-chip-html (pin)
  "A chip for PIN. (set-name . route) pair"
  (format nil "<span class='chip ~(~a~)'>~a</span>"
          (cdr pin) (html-escape (string-downcase (cdr pin)))))

;;; ---------------------------------------------------------------------
;;; SPRT evidence

(defun render-evidence (previous next)
  (if (same-input-p (lambda (s) (getf s :sprt)) previous next)
      :unchanged
      (set-property "evidence" "innerHTML" (js-string (evidence-svg (getf next :sprt))))))

(defun evidence-svg (sprt)
  (svg 640 500
       (append (stream-chart "errors" (getf sprt :errors) sprt 0)
               (stream-chart "slow" (getf sprt :slow) sprt 250))))

(defun stream-chart (label points sprt top)
  "One stream's POINTS as SVG parts, TOP pixels down against SPRT's shared bounds
Walks without bounds yet is a label alone"
  (list* (svg-text 10 (+ top 28) label :ink)
         (if (getf sprt :trimmed) (svg-text 420 (+ top 28) "trimmed" :muted) "")
         (if (getf sprt :upper)
             (walk-parts points sprt top)
             (list (svg-text 120 (+ top 28) "no observations yet" :muted)))))

(defun walk-parts (points sprt top)
  (let ((x (x-scale points sprt))
        (y (y-scale sprt top)))
    (list (bound-line y (getf sprt :upper) "reject: roll back" :reject)
          (bound-line y (getf sprt :lower) "accept: advance" :accept)
          (min-n-line x (getf sprt :min-n) top)
          (walk-line x y points)
          (top-arrow x points sprt top))))

(defun x-scale (points sprt)
  "Pixel x for an observation count N: 0 to the larger of twice min-n and the newest n."
  (let ((x-max (max 1 (* 2 (getf sprt :min-n)) (or (car (first (last points))) 0))))
    (lambda (n) (round (+ 60 (* 560 (/ n x-max)))))))

(defun y-scale (sprt top)
  "Pixel y for an LLR, clamped to one beyond each bound."
  (let ((hi (+ (getf sprt :upper) 1))
        (lo (- (getf sprt :lower) 1)))
    (lambda (llr) (round (+ top 40 (* 180 (/ (- hi (max lo (min hi llr))) (- hi lo))))))))

(defun bound-line (y value label key)
  (format nil "~a~a"
          (svg-line 60 (funcall y value) 620 (funcall y value) 3 key)
          (svg-text 400 (- (funcall y value) 8) label key)))

(defun min-n-line (x min-n top)
  (format nil "<line x1='~d' y1='~d' x2='~d' y2='~d' stroke='~a' stroke-width='3' stroke-dasharray='8 6' data-role='min-n'/>~a"
          (funcall x min-n) (+ top 40) (funcall x min-n) (+ top 220) (color :muted)
          (svg-text (+ 8 (funcall x min-n)) (+ top 236) "earliest verdict" :muted)))

(defun walk-line (x y points)
  (format nil "<polyline fill='none' stroke='~a' stroke-width='4' data-role='walk' points='~{~a~^ ~}'/>"
          (color :ink)
          (mapcar (lambda (point) (format nil "~d,~d" (funcall x (car point)) (funcall y (cdr point))))
                  points)))

(defun top-arrow (x points sprt top)
  "An arrow at the top edge when the newest point is past the top of the scale."
  (let ((newest (first (last points))))
    (if (and newest (> (cdr newest) (+ (getf sprt :upper) 1)))
        (format nil "<polygon fill='~a' data-role='arrow' points='~d,~d ~d,~d ~d,~d'/>"
                (color :reject) (- (funcall x (car newest)) 10) (+ top 50)
                (funcall x (car newest)) (+ top 34) (+ (funcall x (car newest)) 10) (+ top 50))
        "")))

;;; ---------------------------------------------------------------------
;;; SVG helpers

(defun svg (width height parts)
  (format nil "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 ~d ~d'>~{~a~}</svg>"
          width height parts))

(defun svg-rect (x y width height key)
  (format nil "<rect x='~d' y='~d' width='~d' height='~d' rx='6' fill='~a'/>"
          x y width height (color key)))

(defun svg-line (x1 y1 x2 y2 width key)
  (format nil "<line x1='~d' y1='~d' x2='~d' y2='~d' stroke='~a' stroke-width='~d' data-role='~(~a~)'/>"
          x1 y1 x2 y2 (color key) width key))

(defun svg-text (x y text key)
  (format nil "<text x='~d' y='~d' font-size='24' fill='~a'>~a</text>"
          x y (color key) (html-escape text)))

;;; ---------------------------------------------------------------------
;;; Replay row

(defun render-replay (previous next)
  (if (same-input-p (lambda (s) (getf s :replay)) previous next)
      :unchanged
      (replay-js (getf next :replay))))

(defun replay-js (replay)
  (if replay
      (concatenate 'string
                   (set-hidden "replay" nil)
                   (set-hidden "controls" t)
                   (set-text "replay-label" (format nil "Replay of a rehearsal recorded ~a"
                                                    (getf replay :started)))
                   (set-property "scrub" "max" (princ-to-string (1- (getf replay :frames))))
                   (set-property "scrub" "value" (princ-to-string (getf replay :index))))
      (concatenate 'string (set-hidden "replay" t) (set-hidden "controls" nil))))
