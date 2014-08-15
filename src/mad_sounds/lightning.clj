(ns mad-sounds.lightning
  (:require [overtone.live :refer :all]
            [overtone.inst.synth :refer :all]
            [mad-sounds.inst.sampled :refer :all]
            [mad-sounds.inst.synths :refer :all]
            ;;[mad-sounds.launchpad-mini :refer :all]
            ))

(definst ronny [note 60]
  (let [freq (midicps note)
        base-sound (mix [(saw freq) (square freq)])
        filtered (lpf base-sound (mul-add (gendy2 :minfreq 5 :maxfreq 12) 4000 5000))]
    (spring filtered 1000 1)))

(definst ronny2 [note 60 gate 1]
  (let [freq (midicps note)
        base-sound (mix [(saw [freq (* freq 1.1)]) (* (square freq) 0.8) (/ (lf-noise2 (/ freq 2)) 3)])
        filter-mod (mul-add :in (lf-noise0 :freq 25) :mul 1000 :add 7000)
        filtered (lpf base-sound filter-mod)
        with-gendy (+ filtered (/ (gendy2 :minfreq freq :maxfreq (* freq 1.1)) 5))]
    (* (env-gen (adsr 0.03 0.3 0.1 0.2 1) :gate gate) with-gendy)))

;; step 1

(definst swinu [freq 329]
  (saw :freq freq))

;; step 2

(definst swinu2 [note 60]
  (saw :freq (midicps note)))

(midi-poly-player swinu2)

;; step 3

(definst swinu3 [note 60 gate 1]
  (*
   (env-gen (adsr 0.01 0.25 0.3 0.3) :gate gate)
   (saw :freq (midicps note))))

(midi-poly-player swinu3)

;; step 4 : draw the rest of the owl

(definst swinu4 [note 60 gate 1]
  (let [freq (midicps note)
        fm   (sin-osc 7)
        env  (env-gen (adsr 0.01 0.25 0.3 0.3) :gate gate)
        saw-signal (saw (mul-add fm 5 freq))
        sin-signal (sin-osc [freq (+ 3 freq)])]
    (g-verb
     :roomsize 3
     :revtime 0.5
     :in (* env
            (mix [(lpf saw-signal 8000)
                  (/ sin-signal 2)
                  (/ (lf-noise0) 12)])))))

(midi-poly-player swinu5)
;; step 4

(definst swinu4 [note 60 gate 1]
  (*
   (env-gen (adsr 0.01 0.25 0.3 0.3) :gate gate)
   (lpf
    (saw :freq (midicps note))
    8000)))

(midi-poly-player swinu4)


(midi->hz (note :e4))

    (definst ding
      [note 60 velocity 100 gate 1]
      (let [freq (midicps note)
            amp  (/ velocity 127.0)
            snd  (sin-osc freq)
            env  (env-gen (adsr 0.001 0.1 0.6 0.3) gate :action FREE)]
        (* amp env snd)))

(ding)
(stop)

    (def dinger (midi-poly-player ding))
(dinger)
(click-s)
