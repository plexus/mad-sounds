(ns mad-sounds.sessions.aarschot-2023-11-05-lazy-sunday
  (:require
   [clojure.string :as str]
   [mad-sounds.euclid :refer :all]
   [overtone.core :as o :refer :all]
   [vibeflow.midi.jack :as jack]
   [mad-sounds.jack-sequencer :refer :all]))

(boot-server)

(require
 '[overtone.inst.sampled-piano :refer [sampled-piano]]
 '[overtone.inst.drum :as drum])

(jack/connect!
 (jack/client :vibeflow)
 #{["Overtone:out_1" "Built-in Audio Analog Stereo:playback_FL"]
   ["Overtone:out_2" "Built-in Audio Analog Stereo:playback_FR"]
   })

(+loop :kick [0 1 2 3 4 5 7])
(+bang :kick (fn [k s b p] (drum/kick2 :amp 1.5 :sustain 0.2)))
(+loop :hat [0 1/4] :length [0 1/2])
(+bang :hat
  (fn [k s b p]
    (drum/hat3 :amp (if (zero? (-tick b))
                      0.2 0.1))))

(!mute :kick)
state

(bbt->ticks [0 0.25])

(drum/snare)
(!mute :kick)
(play-pause!)
(stop!)

;; loud click at the end
((freesound-sample 26878))
;; no click
((synth (let [buf (play-buf 1 (:id (freesound-sample 26878)))]
          (+ (out 0 buf)
             (out 1 buf))
          )))

(+loop :eights
       (for [[n index] (map vector
                            (concat (euclid-beats 14 4)
                                    (reverse (next (euclid-beats 18 5))))
                            (range))]
         [0
          (if (= 0 (mod index 2))
            (* 1/2 index)
            (+ #_2/3 ; jazz, triplets
               3/4 ;ragtime, 16ths
               (* 1/2 (dec index))))
          0
          {:note (+ 48 n)}]))

(+bang :eights (fn [k s b p]
                 (sampled-piano (:note p))))

(init-sequencer)
(!mute :eights)
(!play)
(rewind!)
state

(def moon
  [[:c4 3/2]
   [:b3 1/2]
   [:a3 1/2]
   [:g3 1/2]
   [nil 1/2]
   [:f3 1]
   [:g3 3/2]
   [:a3 1]
   [:c4 1/2]
   [:b3 2]
   [:a3 1/2]
   [:g3 1/2]
   [:f3 1/2]
   [nil 1/2]
   [:e3 [1 1/4]]])

(defn scorize [notes]
  (reduce
   (fn [acc [note length]]
     (conj (vec (butlast acc))
           (conj (bbt-norm (or (last acc) [])) {:note note})
           (bbt+ (bbt-norm length) (last acc))))
   []
   notes))

(= 2 2.0)

(defn swingize [notes amount]
  (for [n notes]
    (if (= 960 (long (-tick n)))
      (bbt+ n amount)
      n)))

(+bang :moon (fn [k s b p]
               (when (:note p)
                 (sampled-piano (note (:note p))))))
(+trck :moon (swingize (scorize moon) 1/6))

state
(rewind!)
