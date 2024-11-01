(ns vibeflow.synths
  (:use overtone.live))

(definst bell [freq 350
               decaytime {:default 100 :min 0 :max 200}
               attack {:default 0.02 :min 0 :max 0.2}
               release 1.5
               filter-coef {:default 0.5 :min 0.1 :max 1}
               filter-coef2 {:default 0.6 :min 0.1 :max 1}
               cutoff-factor {:default 3 :min 1 :max 20}
               amp 1]
  (let [burst (var-saw freq)]
    (* amp
       8
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
                    hp-factor {:default 3 :min 1 :max 20}
                    lp-factor {:default 2 :min 1 :max 20}
                    deviation {:default 0.03 :min 0 :max 0.2}
                    ]
  (let [freq (for [i (range 4)]
               (* freq (t-rand 1 (+ 1 deviation) 1)))
        mix (lin-lin (sin-osc 0.5) -1 1 0 1)]
    (splay (* amp
              (env-gen (adsr 0.2 0.1 1 0.3) :gate gate :action FREE)
              (+ (* (+ 0.1 mix) (hpf (var-saw (lin-lin (sin-osc (/ 160 60))
                                                       -1 1
                                                       (/ freq 1.02)
                                                       (* freq 1.02)))
                                     (* hp-factor freq)))
                 (* (- 1.1 mix) (lpf (square freq) (* lp-factor freq))))))))

(definst marimba [freq 440 amp 1 gate 1]
  (let [env (env-gen (perc) :gate gate :action FREE)]
    (* amp
       0.3
       env
       (rlpf
        (mix [(var-saw freq)
              (var-saw (* 3 freq))
              (var-saw (* 9.05 freq))])
        (lin-lin env 0 1 (* 3 freq) (* 9 freq))
        1/2))))

(definst dickdale [freq 440 amp 1 gate 1 trigf 25 dur 2]
  (let [trig       (var-saw:kr trigf)
        freq-slide (-> (line:kr freq (/ freq 2) dur FREE)
                       (latch trig))
        sig        (pluck (white-noise)
                          trig
                          :delaytime (/ 1 freq-slide)
                          :decaytime 30)]
    (* amp
       0.2
       (env-gen (asr 0.001 1 0.1) :gate gate :action FREE)
       (rlpf
        sig
        (* (lin-lin (sin-osc (/ freq-slide 4)) -1 1 0.8 1.2) freq-slide 5)
        1/8))))

(definst tonk [freq 200 amp 1 gate 1]
  (* amp
     (env-gen (asr 0 1 0.1) :gate gate :action FREE)
     (lpf
      (pluck (white-noise) :delaytime (/ 1 freq))
      (line 0 5000 0.3))))

(definst moog-bass [freq 110
                    attack      {:min 0 :default 0.01 :max 0.1}
                    decay       {:min 0 :default 0.01 :max 0.3}
                    sustain     {:min 0 :default 0.82 :max 1.5}
                    release     {:min 0 :default 0.24 :max 0.5}
                    amp         {:min 0 :default 1.60 :max 2.5}
                    brightness  {:min 0 :default 3.03 :max 12}
                    osc1-amp    {:min 0 :default 2.53 :max 6}
                    osc2-amp    {:min 0 :default 4.14 :max 6}
                    osc3-amp    {:min 0 :default 1.75 :max 6}
                    lfo-freq    {:min 0 :default 3.16 :max 10}
                    shape       {:min 0 :default 0.07 :max 1}
                    noise       {:min 0 :default 0.57 :max 5}
                    shape-depth {:min 0 :default 0.17 :max 0.25}
                    gate 1]
  (let [env (env-gen (adsr attack decay sustain release) :gate gate :action FREE)]
    (pan2
     (*
      amp
      (mix [(* osc1-amp
               (moog-ff
                :gain 2
                :freq (* brightness freq)
                :in (* 4 (pluck :in (brown-noise)
                                :delaytime (/ 1 freq)))))
            (* env osc2-amp (sin-osc freq))
            (* env osc3-amp
               (rlpf
                (var-saw freq :width (* (lin-lin (sin-osc lfo-freq)
                                                 0 1
                                                 1 (+ 1 shape-depth))
                                        shape))
                #_                (drive-noise

                                   noise)
                (* brightness freq)))])))))

(definst basic-bass [freq 70
                     brightness 1.1
                     attack 0.01
                     decay 0.1
                     sustain 0.7
                     release 0.1
                     amp 0.5
                     gate 1
                     lag 0.1]
  (let [freq (lag:kr freq lag)
        env  (env-gen (adsr attack decay sustain release)
                      :gate gate)
        sig (-> (var-saw freq :width (lin-lin (sin-osc freq) -1 1 0 0.2))
                (rlpf (* brightness freq env) 1/3)
                (* amp env))]
    sig))
