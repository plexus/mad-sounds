(ns mad-sounds.sessions.aarschot-2023-11-03
  (:require
   [vibeflow.midi.core :as midi]
   [clojure.core.match :refer [match]]
   [vibeflow.midi.jack :as jack]
   [overtone.core :as o :refer :all]
   [overtone.synth.stringed :refer :all]
   [overtone.inst.sampled-trumpet :refer :all]))

(require 'overtone.live)

(def midi-in (jack/midi-in-port (jack/client :vibeflow) :in))
(seq (jack/ports (jack/client :vibeflow)))

(jack/connect!
 (jack/client :vibeflow)
 #{["Overtone:out_1" "AG06/AG03 Analog Stereo:playback_FL"]
   ["Overtone:out_2" "AG06/AG03 Analog Stereo:playback_FR"]
   ["orca:out" "vibeflow:in"]})

(sampled-trumpet)
