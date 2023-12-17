(ns mad-sounds.sessions.aarschot-2023-11-06-the-mondays
  (:require
   [clojure.string :as str]
   [mad-sounds.euclid :refer :all]
   [overtone.core :as o :refer :all]
   [casa.squid.jack :as jack]))

(boot-server)
(jack/connect!
 (jack/client :vibeflow)
 #{["Overtone:out_1" "Built-in Audio Analog Stereo:playback_FL"]
   ["Overtone:out_2" "Built-in Audio Analog Stereo:playback_FR"]})

(require '[overtone.inst.synth :as synth])

(stop)
(synth/simple-flute)
(synth/cs80lead)
(synth/supersaw :freq 300)
(synth/ticker)
(synth/ping)
(synth/tb303)
(synth/mooger)
(synth/pad)
(synth/overpad)
(synth/buzz)
(synth/bass)
(synth/daf-bass)
(synth/grunge-bass)
(synth/vintage-bass)
(synth/ks1)
(synth/ks1-demo)
(synth/ks-stringer)
(synth/fm-demo)
(synth/harmonic-swimming)
(synth/whoahaha)
(synth/bubbles)

(overtone.core/stop)

(freesound-sample-pack 1655)
