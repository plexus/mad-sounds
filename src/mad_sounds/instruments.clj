(ns mad-sounds.instruments
  (:require [overtone.studio.scope :refer :all])
  (:require [mad-sounds.launchpad-mini :refer :all])
  (:require [overtone.live :refer :all]))

(def kick-s (freesound 777))
(def click-s (freesound 406))
(def boom-s (freesound 33637))
(def subby-s (freesound 25649))

(definst ding
  [note 60 velocity 100 gate 1]
  (let [freq (midicps note)
        amp  (/ velocity 127.0)
        snd  (sin-osc freq)
        env  (env-gen (adsr 0.001 0.1 0.6 0.3) gate :action FREE)]
    (* amp env snd)))

(definst plexus-kick [freq 120 dur 0.5 width 0.5]
    (let [freq-env (* freq (env-gen (perc 0 (* 0.99 dur))))
          env (env-gen (perc 0.01 dur) 1 1 0 1 FREE)
          sqr (* (env-gen (perc 0 0.01)) (pulse (* 2 freq) width))
          src (sin-osc freq-env)
          drum (+ sqr (* env src))]
      (compander drum drum 0.2 1 0.1 0.01 0.01)))

(plexus-kick)
(ding)
(stop)

(volume 3)
#_(def dinger (midi-poly-player ding))
