(ns mad-sounds.sessions.aarschot-2023-11-08
  (:require
   [clojure.string :as str]
   [mad-sounds.euclid :refer :all]
   [mad-sounds.jack-sequencer :refer :all]
   [overtone.core :as o :refer :all]
   [overtone.synth.stringed :refer :all]
   [vibeflow.midi.jack :as jack]))

(boot-server)
#_(boot-external-server)
(jack/connections (jack/client :vibeflow))

(jack/connect!
 (jack/client :vibeflow)
 #{["Overtone:out_1" "AG06/AG03 Analog Stereo:playback_FL"]
   ["Overtone:out_2" "AG06/AG03 Analog Stereo:playback_FR"]
   })

;; Macroexpanded string synth, manually cleaned up

(defsynth
  string-one
  "A stringed instrument synth with 1 strings mixed and sent thru distortion and
  reverb effects followed by a low-pass filter. Use the pluck-strings and
  strum-strings helper functions to play the instrument. Note: the strings need
  to be silenced with a gate -> 0 transition before a gate -> 1 transition
  activates it. This instrument is transient. When a string becomes silent, it
  will be freed."
  [note      {:default 60, :min 0, :max 127}
   gate      {:default 0}
   dur       {:min 1.0, :default 10.0, :max 100.0}
   decay     {:min 1, :default 30, :max 100}
   coef      {:min -1, :default 0.3, :max 1}
   noise-amp {:min 0.0, :default 0.8, :max 1.0}
   pre-amp   {:min 0.0, :default 6.0, :max 10.0}
   amp       {:min 0.0, :default 1.0, :max 10.0}
   distort'  {:min 0.0, :default 0.0, :max 0.9999999999}
   rvb-mix   {:min 0.0, :default 0.0, :max 1.0}
   rvb-room  {:min 0.0, :default 0.0, :max 1.0}
   rvb-damp  {:min 0.0, :default 0.0, :max 1.0}
   lp-freq   {:min 100, :default 20000, :max 20000}
   lp-rq     {:min 0.1, :default 1.0, :max 10.0}
   pan       {:min -1, :default 0.0, :max 1}
   out-bus   {:min 0, :default 0, :max 100}]
  (let [strings (map (fn [[note gate]]
                       (let [frq (midicps note)
                             nze (* noise-amp (pink-noise))
                             plk (pluck nze gate (/ 1.0 8.0) (/ 1.0 frq) decay coef)]
                         (leak-dc (* plk (env-gen (asr 1.0E-4 1 0.1) :gate gate :action FREE))
                                  0.995)))
                     [[note gate]])
        src (* pre-amp (mix strings))
        k (/ (* 2 distort') (- 1 distort'))
        dis (/ (* src (+ 1 k)) (+ 1 (* k (abs src))))
        vrb (free-verb dis rvb-mix rvb-room rvb-damp)
        fil (rlpf vrb lp-freq lp-rq)]
    (out out-bus (pan2 (* amp fil) pan))))

(demo 5 (pluck (white-noise) 1 0.005 1/240 1.3))
(demo 5 (pluck (pink-noise) 1 0.005 1/240 1.3))
(demo 5 (pan2 (pluck (brown-noise) 1 1/8 1/120 1.3)))
(doseq [i (range 30 35)]
  (demo 5 (pan2 (lpf (pluck (brown-noise) 1 0.01 (/ 1.0 (midicps i)) 1.3) 600)))
  (Thread/sleep 300))


(definst double-bass [note 36 ]
  (let [freq (midicps note)
        src (pluck (brown-noise) 1 1/8 (/ 1.0 (midicps note)) 1.6)
        src2 (pluck (brown-noise) 1 1/8 (/ 1.0 (midicps note) 2) 0.4)
        distort' 0.9
        k (/ (* 2 distort') (- 1 distort'))
        src1 (/ (* src (+ 1 k)) (+ 1 (* k (abs src))))
        ]
    (-> (+ src1 #_src2 )
        (* (env-gen (asr 0.0001 1 0.5) :action FREE))
        (* 1)
        (rlpf (* 2 freq) 0.3)
        (+
         (* 0.5 (rlpf src1 freq 0.5)))
        pan2)))
(stop!)
(stop)
(double-bass 36)

(jack/start-transport! (jack/client :vibeflow))

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
                   (double-bass (:note p)))))

state
(init-sequencer)
(jack/start-transport! (jack/client :vibeflow))
(stop!)
(jack/stop-transport! (jack/client :vibeflow))
