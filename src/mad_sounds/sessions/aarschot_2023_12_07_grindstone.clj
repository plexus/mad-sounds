(ns mad-sounds.sessions.aarschot-2023-12-07-grindstone
  (:require
   [casa.squid.jack :as jack]
   [overtone.at-at :as at]
   [overtone.live :refer :all]
   [overtone.studio.midi :as midi]
   [vibeflow.util :as util]))

(jack/connect!
 (conj
  (util/overtone-conns)
  ["AG06/AG03 Analog Stereo:capture_FL" "Overtone:in_1"]))

(jack/connections)

(demo 100
      (let [[freq has-freq] (pitch (sound-in 0))
            f (midicps (round (cpsmidi freq) 3))]
        (out 0 (pan2 (* has-freq
                        (< freq 1000)
                        (mix [(sin-osc f)
                              (* 0.8 (sin-osc (* 2 f)))
                              (* 0.6 (sin-osc (* 3 f)))
                              (* 0.4 (sin-osc (* 4 f)))]))))))

(pitch-shift)

(stop)

(definst bell1 [note {:default 60 :min 0 :max 120 :step 1}
                mod-interval {:default 12 :min -12 :max 24 :step 1}
                attack {:default 0.001 :min 0 :max 1 :step 0.01}
                decay  {:default 0.2 :min 0 :max 2 :step 0.01}
                sustain  {:default 1 :min 0 :max 2 :step 0.01}
                release {:default 0.5 :min 0 :max 2 :step 0.01}
                depth {:default 1 :min 1 :max 10 :step 0.1}
                gate {:default 1 :min 0 :max 1 :step 1}
                detune {:default 0 :max 1}
                ]
  (let [freq    (midicps note)
        modfreq (midicps (+ note mod-interval))
        mod     (sin-osc :freq (* (+ 1 (* 1/50
                                          (env-gen
                                           (adsr attack decay 0 0)
                                           )))
                                  modfreq))]
    (* (env-gen (adsr attack decay sustain release)
                :gate gate
                :action FREE)

       (mix [(* 0.1 (sin-osc :freq (/ freq 2)))
             (sin-osc :freq freq :phase (* mod depth))
             (* 0.3 (sin-osc :freq (* 5.03 freq)))
             (* 0.4 (sin-osc :freq (* 6.13 freq)))]))))

(util/capture-ctls bell1)

(do
  (bell1 :note 75)
  (Thread/sleep 100)
  (ctl bell1 :gate 0))

(ctl bell1 :decay 0.14 :sustain 0.39 :release 0.28 :depth 1.0 :detune 6/127)

(util/midi-preview :b #'bell1)
(util/midi-ctl :b #'bell1 {21 :depth
                           22 :detune
                           23 :decay
                           24 :sustain
                           25 :release})
(bell1)
(stop)
