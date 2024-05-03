(ns mad-sounds.sessions.aarschot-2024-10-multichannel-expansion
  (:require
   [overtone.live :refer :all]))

;; midicps  - midi note number to Hz
;; saw      - sawtooth wave oscillator
;; moog-ff  - low pass filter based on Moog synths

(demo
 (moog-ff
  (saw (midicps [50 52]))
  [300 1500])
 )

(demo
 (out
  0
  [(moog-ff
    (saw (midicps 50))
    500)
   (moog-ff
    (saw (midicps 52))
    800)])

 )

(demo
 (let [freq 100]
   (pan2
    (mix
     (*
      (for [i [1 2 4 6 8]]
        (sin-osc (* i freq)))
      [1 0.7 0.6 0.2 0.1])
     )))
 )
