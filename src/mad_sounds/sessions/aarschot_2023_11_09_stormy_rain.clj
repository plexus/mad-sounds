(ns mad-sounds.sessions.aarschot-2023-11-09-stormy-rain
  (:require
   [clojure.string :as str]
   [mad-sounds.euclid :refer :all]
   [mad-sounds.jack-sequencer :refer :all]
   [overtone.core :as o :refer :all]
   #_[overtone.synth.stringed :refer :all]
   [net.arnebrasseur.cljack :as jack]
   [net.arnebrasseur.cljack.transport-leader :as transport]))


(boot-server)
#_(boot-external-server)
(jack/connect!
 #{["Overtone:out_1" "AG06/AG03 Analog Stereo:playback_FL"]
   ["Overtone:out_2" "AG06/AG03 Analog Stereo:playback_FR"]
   ["Overtone:out_1" "Built-in Audio Analog Stereo:playback_FL"]
   ["Overtone:out_2" "Built-in Audio Analog Stereo:playback_FR"]
   })

(jack/connections)

(def snare-fat (freesound 122053))
(snare-fat)
(doseq [note [30 32 34]]
  (demo 1
        (let [freq (midi->hz note)
              base (+ (* (env-gen (adsr 0.20 0.95 0 0) :action FREE)
                         (sin-osc freq)
                         0.5)
                      (* (pluck #_(pan2 (brown-noise))
                                (play-buf 2#_(:id snare-fat))
                                :trig 1
                                :maxdelaytime 0.25
                                :delaytime (/ 1 freq)
                                :decaytime 2.3
                                :coef 0.9)))]
          (free-verb
           (+ (lpf base 2500)
              (rlpf base (* 2 freq) 0.1))
           0.3
           :room 0.2
           :damp 1)))
  (Thread/sleep 500))

(def snare-fat (freesound 122053))

(definst bass [note 36]
  (let [freq (midicps note)
        base (+ (* (env-gen (adsr 0.5 0.95 0 0) :action FREE)
                   (sin-osc freq)
                   0.7)
                (* (pluck
                    (play-buf 2 (:id snare-fat))
                    :trig 1
                    :maxdelaytime 0.25
                    :delaytime (/ 1 freq)
                    :decaytime 2.3
                    :coef 0.3)))]
    (free-verb
     (+ (lpf base 800)
        (rlpf base (* 2 freq) 0.1))
     0.3
     :room 0.2
     :damp 1)))

(bass 38)
(init-sequencer)
(transport/initialize!)

(def progression [[:c2 :m7]
                  [:f2 :dom7]
                  [:bb2 :major7]
                  [:eb2 :major7]
                  [:a1 :m7-5]
                  [:d2 :dom7]
                  [:g2 :m6]
                  [:g2 :m6]])

(+loop :bass
       (->> progression
            (mapcat
             (fn [idx [root ch]]
               (let [[i iii v vii] (chord root ch)
                     [n1 n2 n3 n4] (rand-nth [#_[i iii i v] [i v iii i]])]
                 (if (even? idx)
                   [[idx 0 0 {:note n1}]
                    [idx 2 0 {:note n2}]]
                   [[idx 0 0 {:note n3}]
                    [idx 2 0 {:note n4}]])))
             (range))))

(+bang :bass (fn [k s b p]
               (at (+ (now) (rand-int 20))
                   (bass (:note p)))))

state
(jack/start-transport!)
(jack/stop-transport!)
(jack/seek-transport! 0)
(stop!)

(swap! transport/timing assoc :beats-per-minute 90)

(base)
