(ns mad-sounds.sessions.snb-2024-04-07
  (:use overtone.live))

(definst rusty [freq 300
                gate 1]
  (* (env-gen (perc) :gate gate :action FREE)
     (moog-ff
      (+ (* 0.2 (var-saw :width 1 :freq (repeatedly 8 #(+ freq (midiratio (srand 0.1))))))
         (square freq))
      (* 30 freq))))

(pplay
 :rusty
 (pbind
  {:instrument rusty
   :degree [:i 2 3 4 :v 4 3 2 1 :_]
   :mtranspose (pdo (repeat 10 (srand 7)))
   :dur (concat (repeat 8 1) [3 1])}
  INF))

(definst brock [freq 150
                gate 1]
  (* (env-gen (asr :attack 0.01 :release 0.1) :gate gate :action FREE)
     (lpf
      (pluck (brown-noise) :delaytime (/ 1 freq) :decaytime 2)
      (* 3 freq)))
  )

(defn prepeat [pattern n]
  (mapcat #(repeat n %) pattern))

(prepeat (pbind {:a [1 2 3]} 2) 4)

(pplay
 :rusty
 (prepeat
  (pbind
   {:type :chord
    :strum 0.66
    :instrument brock
    :inversion 1
    :chord-size 3
    :degree [:V :I :IV :VI
             :II :III :V :I]
    :root 2
    :octave 2
    :dur 4}
   INF)
  2))
(stop)
(degree->int :V)

(brock)
(stop)

(demo (* (env-gen (adsr))
         (sin-osc 200 )))
