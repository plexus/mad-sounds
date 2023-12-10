(ns mad-sounds.sessions.aarschot-2023-12-09-violin
  (:require
   [casa.squid.jack :as jack]
   [overtone.live :refer :all]
   [vibeflow.util :as util]))

(jack/connect!
 [["SuperCollider:out_1" "AG06/AG03 Analog Stereo:playback_FL"]
  ["SuperCollider:out_2" "AG06/AG03 Analog Stereo:playback_FR"]
  ["SuperCollider:out_1" "Wolf Spectrum:in1"]
  ["SuperCollider:out_2" "Wolf Spectrum:in2"]])

(jack/connections)

(jack/ports)

(midi->hz 85)

(clojure.repl/apropos "porta")


(definst v [note {:default 60 :max 85}
            cutoff {:default 3 :max 12}
            gate 1]
  (let [freq (midicps note)]
    (->
     (* (var-saw :freq freq)
        (env-gen (adsr 1.5 1.5 0.8 1) :gate gate :action FREE))
     (lpf :freq (* cutoff freq)))))

(demo (var-saw :freq 400))

(v)

(stop)



(util/midi-preview :v #'v)
(util/midi-ctl :v #'v {21 :cutoff})
