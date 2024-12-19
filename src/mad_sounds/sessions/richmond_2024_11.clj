(ns mad-sounds.sessions.richmond-2024-11
  (:use overtone.live)
  (:require [overtone.helpers.lib :refer [defunk]]))

(def cent (java.lang.Math/pow 2 1/1200))

(definst strings [freq 400
                  gate 1]
  (pan2
   (*
    (env-gen (asr 0.3 1 0.3) :gate gate :action FREE)
    (lpf
     (+
      (pulse
       (* freq cent)
       (+ 0.5 (/ (var-saw (latch (white-noise:kr) (impulse 20))) 10)))
      (pulse
       (/ freq cent)
       (+ 0.5 (/ (var-saw (latch (white-noise:kr) (impulse 20))) 10))))
     (* 20 freq #_(env-gen (envelope [0.1 1 0.8 0.1] [0.3 1.5 0.3])))))))

(strings)
(stop)

(cents)

(ploop :s
       {:type :note
        :instrument strings
        :dur 4
        :degree [1 5 4 1]
        :octave 4})

(demo
 20
 (let [freq (lin-lin (latch (white-noise:kr) (impulse 10))
                     -1 1
                     200 800)]
   (pan2
    (*
     (env-gen (asr 0.3 1 0.3) )
     (lpf
      (+
       (pulse
        (* freq cent)
        (+ 0.5 (/ (var-saw (latch (white-noise:kr) (impulse 20))) 10)))
       (pulse
        (/ freq cent)
        (+ 0.5 (/ (var-saw (latch (white-noise:kr) (impulse 20))) 10))))
      (* 20 freq #_(env-gen (envelope [0.1 1 0.8 0.1] [0.3 1.5 0.3])))))))
 )

(defunk my-fn "" [x 1 y 2]
  (+ x y))

(my-fn)
;; => 3
(my-fn 2)
;; => 4
(my-fn :y 3)
;; => 4
