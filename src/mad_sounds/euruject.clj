(ns mad-sounds.euruject
  (:require [mad-sounds.launchpad-mini :refer :all])
  (:require [overtone.inst.sampled-piano :refer :all])
  (:require [overtone.live :refer :all]))


(definst c-hat [amp 0.8 t 0.04]
  (let [env (env-gen (perc 0.001 t) 1 1 0 1 FREE)
        noise (white-noise)
        sqr (* (env-gen (perc 0.01 0.04)) (pulse 880 0.2))
        filt (bpf (+ sqr noise) 9000 0.5)]
    (* amp env filt)))

(definst weirdos [freq 440 gate 1]
  (let [noise (lf-noise1 3)
        saws  (mul-add (lf-saw [5 5.123]) 3 80)
        freq  (midicps (+ (mul-add noise 24 saws)))
        src   (* 0.4 (sin-osc freq))
        env   (env-gen (adsr 0.001 0.1 0.6 0.3) gate :action FREE)]
      (* (comb-n src 1 0.3 2) env)))

(weirdos)
(stop)

(sampled-piano)

(lp-bind-octave c-hat 7)

(kick)
(lp-bind kick 0 0)
(lp-bind snare 0 1)
(lp-bind c-hat 1 0)
(c-hat)
(volume 3)
