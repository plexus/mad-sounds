(ns mad-sounds.sessions.richmond-2023-10
  (:require
   [overtone.live :refer :all]
   [overtone.inst.synth :as ois]
   [overtone.inst.drum :as drum]
   [vibeflow.midi.core :as midi]
   [vibeflow.midi.jack :as jack]
   [clojure.core.match :refer [match]]
   [vibeflow.drumcircle :as drumcircle]
   [vibeflow.drumcircle.ui :as drumcircle-ui]))

(def client (jack/client :vibeflow))
(def midi-in (jack/midi-in-port client :in))

(jack/connect!
 client
 #{["Overtone:out_1" "Built-in Audio Analog Stereo:playback_FL"]
   ["Overtone:out_2" "Built-in Audio Analog Stereo:playback_FR"]
   ["orca:out" "vibeflow:in"]})

(drumcircle/start-sequencer)
(drumcircle-ui/start-ui)

(def kick (freesound 16699))
(def snare (freesound 216045))
(def hat-closed (freesound 506453))
(def hat-open (freesound 183105))

(defn handle [e]
  (match e
    [9 :note-on 35 _] (kick)
    [9 :note-on 38 _] (snare)
    [9 :note-on 42 _] (hat-closed)
    [9 :note-on 46 _] (hat-open)
    [_ :note-off _ _] nil
    [a b c d] (println "unhandled" [a b c d])
    ))

;; 38 (drum/snare)
;; 42 (drum/closed-hat)
;; 46 (drum/open-hat)

(jack/register client :process ::monitor
               (fn [client frames]
                 (when-let [events (seq (jack/read-midi-events midi-in))]
                   (doseq [[e t] events]
                     (let [[_ et :as me] (midi/event e)]
                       #_(prn me)
                       (handle me))))
                 true))

(stop)
