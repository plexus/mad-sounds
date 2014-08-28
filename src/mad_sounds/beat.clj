(ns mad-sounds.beat
  (:require [overtone.live :refer :all])
  (:require [mad-sounds.launchpad-mini :as lp]))

(defonce root-bus (control-bus))
(defonce beat-bus (control-bus))
(defonce beat-count-bus (control-bus))

(defonce beat-trigger (trig-id))

(+ 2 2)

(defsynth root-synth
  "Put a steady 100Hz pulse on the root-bus"
  [rate 100]
  (out:kr root-bus (impulse:kr rate)))

(defsynth beat-synth
  "Put a steady per-beat pulse on the beat-bus"
  [div 30]
  (out:kr beat-bus (pulse-divider (in:kr root-bus) div)))

(defsynth beat-count-synth
  "Put the beat count on the beat-count-bus"
  []
  (out:kr beat-count-bus (pulse-count (in:kr beat-bus))))

(defsynth trigger-beat-synth
  "Trigger the beat-trigger whenever a beat happens"
  []
  (send-trig (in:kr beat-bus) beat-trigger (+ (in:kr beat-count-bus) 1)))

(fn [] )
#()
#{:foo :bar :this :is 'a 'set}

(defn start-the-beat!
  ([] (start-the-beat! 140))
  ([bpm] (start-the-beat! bpm 30))
  ([bpm divider]
     (let [bps (/ bpm 60)
           root-rate (*  bps divider)]
       {:root (root-synth root-rate)
        :beat (beat-synth divider)
        :count (beat-count-synth)
        :trigger (trigger-beat-synth)})))

(start-the-beat!)

(use 'overtone.inst.drum)
(use '[overtone.helpers.lib :only [uuid]])

(defonce kick-uuid (uuid))

(stop)
(on-trigger beat-trigger (fn [beat]
                           (if
                               (=  (rem beat 2) 0)
                             (kick)
                             (snare)
                             )) kick-uuid)
