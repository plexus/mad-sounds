(ns mad-sounds.sessions.aarschot-2023-11-10-fixing-stuff)

(use 'overtone.live)

(use 'overtone.synth.stringed)

(defonce high-hats-g (group))
(defsynth high-hats [out-bus 0 beat-bus 0 beat-trg-bus 0 note-buf 0 seq-buf 0 beat-num 0 num-steps 0
                     attack 0.001 release 0.1 mix 0 room 0 damp 0 amp 1]
  (let [cnt      (in:kr beat-bus)
        beat-trg (in:kr beat-trg-bus)
        note     (buf-rd:kr 1 note-buf cnt)
        bar-trg (and (buf-rd:kr 1 seq-buf cnt)
                     (= beat-num (mod cnt num-steps))
                     (not= note 0)
                     beat-trg)

        freq (midicps note)
        c (pm-osc:ar freq (* freq (t-rand 0.25 2.0 bar-trg)) (t-rand 0.1 (* 2 Math/PI) bar-trg))
        e (env-gen:kr (env-perc attack release) bar-trg)
        src (/ (* c e 0.125) 2)
        src (free-verb src :mix mix :room room :damp damp)]
    (out out-bus [(* amp src) (* amp src)])))
overtone.sc.ugen-collide/abs
(high-hats)

(high-hats [:head high-hats-g]
           :amp 0.7
           :mix 0
           :room 2
           :damp 0.6
           :note-buf 0
           :seq-buf 0
           :beat-bus     0
           :beat-trg-bus 0 :num-steps 32 :beat-num 0)

(:require
 [clojure.string :as str]
 [mad-sounds.euclid :refer :all]
 [mad-sounds.jack-sequencer :refer :all]
 [overtone.core :as o :refer :all]
 #_[overtone.synth.stringed :refer :all]
 [net.arnebrasseur.cljack :as jack]
 [net.arnebrasseur.cljack.transport-leader :as transport])

abs

(doseq [ns
        (for [ns (.split #"\R" (slurp "/tmp/nss"))]
          (-> ns
              (.replace "./" "")
              (.replace ".clj" "")
              (.replace "/" ".")
              (.replace "_" "-")
              symbol))]
  (try
    (println "Loading " ns)
    (require ns)
    (catch Exception e
      (println e))))
