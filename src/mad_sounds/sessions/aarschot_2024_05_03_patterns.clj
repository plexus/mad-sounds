(ns mad-sounds.sessions.aarschot-2024-05-03-patterns
  (:require
   [mad-sounds.euclid :refer :all]
   [overtone.live :refer :all]
   [overtone.studio.transport :as transport]))

(definst mucklewain [freq 440 gate 1 amp 1]
  (splay
   (* (env-gen (adsr :decay 0.2 :sustain 0.5) :gate gate :action FREE)
      amp
      (rlpf (square (freq-spread freq 4))
            (* 5 freq)
            0.1))
   :spread (env-gen (envelope [1 0 1] [0.05 0.05]))
   :level-comp? false))

(definst gloria [freq 265
                 gate 1.0
                 amp 1]
  (splay
   (* (env-gen (adsr :attack 0.009 :decay 0.2 :sustain 0.5) :gate gate :action FREE)
      amp
      (+ (* 0.5 (sin-osc freq))
         (pluck (brown-noise)
                :maxdelaytime 1
                :delaytime (/ 1 (freq-spread freq 3 0.5)))))
   :level-comp? false))

(definst snare [rel 0.3
                amp 1]
  (pan2
   (* (env-gen (perc :release rel) :action FREE)
      amp
      (pluck
       (white-noise)
       :delaytime (env-gen
                   (envelope [35/1000 25/1000 10/1000 20/1000]
                             (repeat 3 (/ rel 3))))
       :coef (env-gen (envelope [-0.1 -0.2] [rel]))))))

(definst tick [amp 1]
  (pan2
   (* (env-gen (perc :release 0.08) :action FREE)
      amp
      (hpf
       (pluck
        (white-noise)
        :delaytime 0.018
        :coef 0.1)
       2000))))


(def p1
  {:instrument gloria
   :degree     [5 3 2 4 1 9 10]
   :dur        [1 1/2 1 1/2 1/2 1/2 2]})

(def p2
  {:instrument gloria
   :degree     [6   9   4 2   1  11  11  11 0]
   :dur        [1 1/2 1/2 1   1 1/3 1/3 1/3 2]})

(do
  (pplay ::m
         (repeat [(ppad (pbind p1) 8)
                  (ppad (pbind p1) 8)
                  (ppad (pbind p2) 8)
                  (ppad (pbind p2) 8)])
         {:quant 8})

  (pplay ::p
         {:instrument mucklewain
          :amp (pwhite 0.05 0.08)
          :degree [1 1 4 5 1 6 5 4]
          :dur (repeat 2)}
         {:quant 8
          :offset 32})

  (pplay ::tick
         {:instrument tick
          :amp (pwhite 0.2 0.4)
          :dur (repeat [1 1 1 1 1 1 1/2 1/2 1/3 1/3 1/3])}
         {:quant 8
          :offset 64}))

(premove ::m)
(premove ::tick)
(premove ::p)
