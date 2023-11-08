(ns mad-sounds.sessions.sequencer-demo
  (:require
   [clojure.string :as str]
   [mad-sounds.jack-sequencer :refer :all]
   [overtone.core :as o :refer :all]
   [overtone.synth.stringed :refer :all]
   [vibeflow.midi.jack :as jack]))

(boot-server)

(jack/connect!
 (jack/client :vibeflow)
 #{["Overtone:out_1" "AG06/AG03 Analog Stereo:playback_FL"]
   ["Overtone:out_2" "AG06/AG03 Analog Stereo:playback_FR"]
   })

(def g (guitar))

(init-sequencer)

;; Walking bass
(+loop :bass
       ;; [bar beat tick params]
       [[0 0 0 {:note 36}]
        [0 2 0 {:note 43}]
        [1 0 0 {:note 45}]
        [1 2 0 {:note 41}]
        [2 0 0 {:note 46}]
        [2 2 0 {:note 53}]
        [3 0 0 {:note 43}]
        [3 2 0 {:note 39}]
        [4 0 0 {:note 33}]
        [4 2 0 {:note 39}]
        [5 0 0 {:note 42}]
        [5 2 0 {:note 38}]
        [6 0 0 {:note 43}]
        [6 2 0 {:note 50}]
        [7 0 0 {:note 46}]
        [7 2 0 {:note 43}]]
       ;; :length [4] ; implicit
       )

(+bang :bass (fn [k s b p] (guitar-pick g 0 (- (:note p) 36))))

(!play) ;; toggles
(!mute :bass)
