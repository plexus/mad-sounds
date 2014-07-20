;; (ns mad-sounds.pads
;;   (:require [overtone.live :as ot])
;;   (:require [overtone.inst.drum :as drum])
;;   (:require [overtone.studio.scope :as scope]))

(ns mad-sounds.viz
  (:use
   overtone.live
   overtone.studio.scope))

(scope)
(def b (buffer 44100))

(mk-sc)

;; We also created an sc synth which looked as follows:

(defsynth my-saw []
  (record-buf (lf-saw 440) b))

(my-saw)

;; There are two interesting things happening in this synth, first we generate a saw wave with the ugen lf-saw and second we output it to the record-buf ugen for which we specify our buffer as a parameter. This has the effect of piping the lf-saw wave directly into the buffer.

;; Now, to view it using Overtone's scope we need to pull in the scope as part of our namespace declaration:


;; We can now define our buffer and synth as described above. Once we have the buffer and synth loaded up we can now just call the synth:

(my-saw)

;; This will populate the buffer with the output of the lf-saw ugen.

;; OK, and now for the super fun part:

(scope :buf b)

;; This will open up a new window with the contents of the buffer plotted. Wasn't that easy!

;; The fun doesn't end there either, we can update the contents of buffer b with the output of sin-osc wave:

(defsynth my-sin []
 (record-buf (sin-osc 440) b :action :free, :loop 0))

(my-sin)

;; Hey presto! The scope has automatically updated with the new contents. How lovely.

;; This live updating behaviour isn't just limited to buffers, we can also scope busses:

(scope :bus 0)

;; This will scope all sound output from the server. In fact this is so useful it's the default behaviour for no params:

(scope) ;=> same as (scope :bus 0)

;; Try opening a new scope set for bus 0 and create your favourite synth and play it:

(definst beep [] (sin-osc 440))
(beep)

;; See the live wave in all its glory :-)

;; Hope this helps,

;; please feel free to ask as many questions as you can!

;; Sam

;; P.S. This will *only* work with code from the latest master branch on Github as of yesterday
.
