(ns mad-sounds.sessions.aarschot-2023-12-09-violin
  (:require
   [casa.squid.jack :as jack]
   [overtone.live :refer :all]
   [vibeflow.util :as util]))

(jack/connect!
 [["SuperCollider:out_1" "AG06/AG03 Analog Stereo:playback_FL"]
  ["SuperCollider:out_2" "AG06/AG03 Analog Stereo:playback_FR"]
  ["SuperCollider:out_1" "AG06/AG03 Pro:playback_AUX0"]
  ["SuperCollider:out_2" "AG06/AG03 Pro:playback_AUX1"]
  ["SuperCollider:out_1" "Wolf Spectrum:in1"]
  ["SuperCollider:out_2" "Wolf Spectrum:in2"]])

(jack/connections)

(jack/ports)

(midi->hz 85)

(clojure.repl/apropos "porta")

(clojure.repl/apropos "reverb")

(ctl v :hpf-freq 4.535433070866142 :lpf-freq 10.01574803149606 :attack 0.1181102362204724 :eq1-freq 1.511811023622047 :eq1-q 0.5196850393700787 :eq1-gain 22.83464566929134 :eq2-freq 2.582677165354331 :eq2-q 0.8740157480314961 :eq2-gain 14.96062992125984)

(definst v [note {:default 60 :max 85}
            hpf-freq {:default 4.5 :max 12}
            lpf-freq {:default 11 :max 12}
            attack {:default 0.12 :min 0 :max 3}
            decay  {:default 0.3 :min 0 :max 2}
            sustain  {:default 1 :min 0 :max 2}
            release {:default 0.5 :min 0 :max 2}
            vibrato-freq {:default 6 :min 4 :max 12}
            vibrato-onset {:default 1 :max 3}
            vibrato {:default 0.2 :max 5}
            eq1-freq {:default 4 :max 4 }
            eq1-q {:default 0.5 :max 3}
            eq1-gain {:default 4 :min -100 :max 100}
            eq2-freq {:default 6 :max 10}
            eq2-q {:default 0.87 :max 3}
            eq2-gain {:default 4 :min -100 :max 100}
            gate 1]
  (let [freq (midicps note)]
    (-> (+ (* (pink-noise)
              0.0001
              (env-gen (adsr 0.1 0.1 0 0))
              (mid-eq freq 1 10))
           (* (+ (var-saw :freq freq)
                 (* 1.5 (pluck (white-noise) :delaytime (/ 1 freq)))
                 (* 0.8 (var-saw :freq (* 0.99 freq)))
                 )
              0.5
              (env-gen (adsr attack decay sustain release)
                       :gate gate
                       :action FREE)
              (+ 1 (* vibrato
                      (sin-osc :freq vibrato-freq)
                      (env-gen (asr vibrato-onset 1 0.3))))))
        (mid-eq (* eq1-freq freq) eq1-q eq1-gain)
        (mid-eq (* eq2-freq freq) eq2-q eq2-gain)
        (hpf :freq (* hpf-freq freq))
        (lpf :freq (* lpf-freq freq))
        (free-verb 1)
        )))


(definst v [note {:default 60 :max 85}
            hpf-freq {:default 4.5 :max 12}
            lpf-freq {:default 10.4 :max 12}
            attack {:default 0.12 :min 0 :max 3}
            decay  {:default 0.3 :min 0 :max 2}
            sustain  {:default 1 :min 0 :max 2}
            release {:default 0.5 :min 0 :max 2}
            vibrato-freq {:default 6 :min 4 :max 12}
            vibrato-onset {:default 1 :max 3}
            vibrato {:default 0.2 :max 5}
            h1-gain {:default 1 :max 24}
            h2-gain {:default 1 :max 24}
            h3-gain {:default 1 :max 24}
            h4-gain {:default 1 :max 24}
            h5-gain {:default 1 :max 24}
            h6-gain {:default 1 :max 24}
            h7-gain {:default 1 :max 24}
            h8-gain {:default 1 :max 24}
            hrq {:default 1 :max 10}
            gate 1]
  (let [freq (midicps note)]
    (-> (* (+ (var-saw :freq freq)
              )
           (env-gen (adsr attack decay sustain release)
                    :gate gate
                    :action FREE)
           (+ 1 (* vibrato
                   (sin-osc :freq vibrato-freq)
                   (env-gen (asr vibrato-onset 1 0.3)))))
        (mid-eq freq hrq h1-gain)
        (mid-eq (* 2 freq) hrq h2-gain)
        (mid-eq (* 3 freq) hrq h3-gain)
        (mid-eq (* 4 freq) hrq h4-gain)
        (mid-eq (* 5 freq) hrq h5-gain)
        (mid-eq (* 6 freq) hrq h6-gain)
        (mid-eq (* 7 freq) hrq h7-gain)
        (mid-eq (* 8 freq) hrq h8-gain)
        (hpf :freq (* hpf-freq freq))
        (lpf :freq (* lpf-freq freq))
        )))

(demo (pan2 (+ (var-saw :freq 440)
               (var-saw :freq 432))))

(v)

(stop)

(util/capture-ctls #'v)
;; => (ctl v :hpf-freq 204/127 :lpf-freq 1392/127)

(ctl v :hpf-freq 204/127 :lpf-freq 864/127 :vibrato 25/127)
(double 204/127)
;; => 10.46456692913386

(util/midi-preview :v #'v)
(util/midi-ctl :v #'v
               {21 :eq1-freq
                22 :eq1-q
                23 :eq1-gain
                24 :eq2-freq
                25 :eq2-q
                26 :eq2-gain

                27 :attack #_:hpf-freq
                28 :lpf-freq}
               #_{21 :cutoff
                  22 :vibrato
                  23 :attack
                  })
(util/midi-ctl :v #'v
               {21 :h1-gain
                22 :h2-gain
                23 :h3-gain
                24 :h4-gain
                25 :h5-gain
                26 :h6-gain
                27 :h7-gain
                28 :h8-gain
                }
               #_{21 :cutoff
                  22 :vibrato
                  23 :attack
                  })
