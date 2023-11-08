(ns mad-sounds.sessions.dulles-airport-2023-10-30
  (:require
   [vibeflow.midi.core :as midi]
   [clojure.core.match :refer [match]]
   [vibeflow.midi.jack :as jack]
   [overtone.core :as o :refer :all]
   [overtone.synth.stringed :refer :all]))

(require 'overtone.live)

(def midi-in (jack/midi-in-port (jack/client :vibeflow) :in))
(seq (jack/ports (jack/client :vibeflow)))

(jack/connect!
 (jack/client :vibeflow)
 #{["Overtone-66:out_1" "Built-in Audio Analog Stereo:playback_FL"]
   ["Overtone-66:out_2" "Built-in Audio Analog Stereo:playback_FR"]
   ["orca:out" "vibeflow:in"]})

(def g (guitar))
(guitar-strum g :E 0.5)
(reset! overtone.sc.machinery.server.comms/osc-debug* false)

(o/boot-external-server)

#_(o/connect-external-server 12947)
(let [note (- 60 39)]
  [(quot note 5) (mod note 5)])
(defn handle [e]
  (match e
    [0 :note-on note _]
    (let [note (- note 39)]
      (guitar-pick g (quot note 5) (mod note 5) (now)))
    [1 :note-on note _]
    (let [note (- note (rand-nth [36 35]))]
      (guitar-pick g (quot note 5) (mod note 5) (+ 120 (rand-int 5) (now))))
    [2 :note-on note _]
    (let [note (- note (rand-nth [30 31]))]
      (guitar-pick g (quot note 5) (mod note 5) (+ 240 (rand-int 5) (now))))
    [_ :note-off _ _] nil
    [a b c d] (println "unhandled" [a b c d])
    ))

;; ....................
;; ..4C8...............
;; ...38TG.AC.G.C......
;; .2.3.C..............
;; .D4..J..............
;; ..:03C..............
;; .J...J..............
;; ..:13C..............
;; .J...J..............
;; ..:24C..............
;; ....................

(jack/register
 (jack/client :vibeflow)
 :process ::monitor
 (fn [client frames]
   (when-let [events (seq (jack/read-midi-events midi-in))]
     (doseq [[e t] events]
       (let [[_ et :as me] (midi/event e)]
         #_(prn me)
         (handle me))))
   true))

(demo
 (mouse-x))

(defsynth foo [freq 440]
  (abs (sin-osc freq)))



(set! *print-meta* true)
(overtone.sc.synth/pre-synth '[freq 440] '(sin-osc freq))
(synth)

(definst hat-demo
  [amp    {:default 0.3 :min 0.001 :max 1 :step 0.01}
   t      {:default 0.3 :min 0.1 :max 1.0 :step 0.01}]
  (let [low (lpf (white-noise) (mouse-x 3000 12000))
        hi (hpf low (mouse-y 1000 8000))
        env (line 1 0 t :action FREE)]
    (* amp env hi)))
