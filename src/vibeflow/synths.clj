(ns vibeflow.synths
  (:use overtone.live))

(definst bell [freq 350
               decaytime {:default 100 :min 0 :max 200}
               attack {:default 0.02 :min 0 :max 0.2}
               release 1.5
               filter-coef {:default 0.5 :min 0.1 :max 1}
               filter-coef2 {:default 0.6 :min 0.1 :max 1}
               cutoff-factor {:default 3 :min 1 :max 7}
               amp 1]
  (let [burst (var-saw freq)
        ]
    (* amp
       (env-gen (perc attack release) :action FREE)
       (rlpf
        (mix
         [(pluck burst
                 :delaytime (/ 1 freq)
                 :decaytime decaytime
                 :coef filter-coef)
          (* (line 0 1 6)
             (pluck burst
                    :delaytime (/ 1 (+ freq 2))
                    :decaytime decaytime
                    :coef filter-coef2))
          (* (line 0.1 0.6 6)
             (pluck burst
                    :delaytime (/ 1 (+ freq 10))
                    :decaytime decaytime
                    :coef filter-coef2))
          (* (line 0 1 4)
             (pluck burst
                    :delaytime (/ 1 (- freq 1))
                    :decaytime decaytime
                    :coef filter-coef2))])
        (* cutoff-factor freq)))))

(definst churchbell [freq 350
                     decaytime 6
                     attack 0
                     release 5
                     gate 1
                     amp 1]
  (let [burst (var-saw freq)
        filter-coef 0.3
        filter-coef2 0.4]
    (* amp
       (env-gen (asr 0 1 0.3) :gate gate :action FREE)
       (rlpf
        :in (+
             (pluck burst
                    :delaytime (/ 1 freq)
                    :decaytime decaytime
                    :coef filter-coef)
             (* (line:kr 0.1 0.3 4)
                (pluck burst
                       :delaytime (/ 1 (* 1.01 freq))
                       :decaytime decaytime
                       :coef filter-coef2))
             (* 0.3
                (pluck burst
                       :delaytime (/ 1 (* 2.15 freq))
                       :decaytime decaytime
                       :coef filter-coef2)))
        :freq (line (* 5 freq) (* 1.3 freq) 1.5)))))

(definst wobblepad [freq 300
                    amp 1
                    gate 1
                    hp-factor 3
                    lp-factor 2]
  (let [freq (for [i (range 4)]
               (* freq (rand-in-range 1 1.03)))
        mix (lin-lin (sin-osc 0.5) -1 1 0 1)]
    (splay (* amp
              (env-gen (adsr 0.2 0.1 1 0.3) :gate gate :action FREE)
              (+ (* (+ 0.1 mix) (hpf (var-saw (lin-lin (sin-osc 4)
                                                       -1 1
                                                       (/ freq 1.02)
                                                       (* freq 1.02)))
                                     (* hp-factor freq)))
                 (* (- 1.1 mix) (lpf (square freq) (* lp-factor freq))))))))
