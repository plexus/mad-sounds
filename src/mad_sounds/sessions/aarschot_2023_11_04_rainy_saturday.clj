(ns mad-sounds.sessions.aarschot-2023-11-04-rainy-saturday
  (:require
   [clojure.string :as str]
   [mad-sounds.euclid :refer :all]
   [overtone.core :as o :refer :all]
   [overtone.inst.percussion :refer :all]
   [overtone.inst.sampled-piano :refer [sampled-piano]]
   [vibeflow.midi.jack :as jack]))

(piano)

(boot-server)

(jack/ports (jack/client :vibeflow) #{:audio})
(jack/connections (jack/client :vibeflow))

(jack/connect!
 (jack/client :vibeflow)
 #{["Overtone:out_1" "Built-in Audio Analog Stereo:playback_FL"]
   ["Overtone:out_2" "Built-in Audio Analog Stereo:playback_FR"]
   })

(defn arpeggio-7th [n]
  (do
    (sampled-trumpet n)
    (at (+ 500 (now))
        (sampled-trumpet (+ 4 n)))
    (at (+ 1000 (now))
        (sampled-trumpet (+ 7 n)))
    (at (+ 1500 (now))
        (sampled-trumpet (+ 11 n)))))
(ctl sampled-trumpet :gate 0)

(arpeggio-7th 52)
(arpeggio-7th 56)
(arpeggio-7th 57)
(arpeggio-7th 62)

(nth-interval :minor 2)
(nth-interval :minor 2)

(defonce !stop (atom false))
(reset! !stop false)
(reset! !stop true)

(let [m (metronome 320)]
  (let [s (euclid-seq 13 5)]
    ((fn cnt [s]
       (when-not @!stop
         (apply-at (m (first s))
                   (fn []
                     (prn (first s))
                     ((rand-nth [knife-hit-10 knife-hit-11 knife-hit-12]))
                     (cnt (next s))))))
     s)))

(volume 0.8)

(let [notes (map (partial + 45) (take 45 (euclid-seq 23 13)))]
  (dotimes [_ 3]
    (dotimes [i 23]
      (when (some #{i} (euclid-beats 23 13))
        (sampled-piano (rand-nth (take 15 notes))))
      (Thread/sleep (rand-nth [175 #_350])))

    (Thread/sleep 700)

    (dotimes [i 21]
      (when (some #{i} (euclid-beats 21 13))
        (sampled-piano (rand-nth (take 9 (drop 12 notes)))))
      (Thread/sleep (rand-nth [125 #_250])))

    (Thread/sleep 500)

    (dotimes [i 23]
      (when (some #{i} (euclid-beats 23 13))
        (sampled-piano (rand-nth (take 13 (drop 5 notes)))))
      (Thread/sleep (rand-nth [150 #_300])))

    (Thread/sleep 600)
    ))

(Thread/sleep (rand-nth [250 500 125]))
