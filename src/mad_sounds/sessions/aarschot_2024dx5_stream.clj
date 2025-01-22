(ns mad-sounds.sessions.aarschot-2024dx5-stream
  "Code is at
   https://github.com/plexus/mad-sounds/blob/master/src/mad_sounds/sessions/aarschot_2024dx5_stream.clj"
  (:require
   [overtone.live :refer :all]
   [vibeflow.synths :as s]
   [vibeflow.util :as u]))

(def bell1
  {:note [:g :f :e :d :c :d :e :c]
   :dur [3/2 1/2 1 1  1 1 1 1]})

(def bell2
  {:note [:d :e :f :d :e :d :c :b4 :c]
   :dur [1/2 1/2 1/2 1/2 3/2 1/2 1 1 2]})


(def bell3
  {:note [:d :e :f :d :e :f :g :d :e :f :g :a :b :c6 :b :a :g]
   :dur [3/2 1/2 1 1 3/2 1/2 1 1 1/2 1/2 1 1/2 1/2 1 1 1 2]})

(def bell4
  {:note [:a :a :a :a :g :f :e :d :c]
   :dur [1/2 1/2 1/2 1/2 3/2 1/2 1 1 2]})

(pplay
 :bell
 (mapcat pbind [bell1 bell2 bell3 bell1 bell4])
 {:proto {:instrument fluflu}})
(stop)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def kick909 (freesound 145775))
(def ch909 (freesound 26520))
(def clap (freesound 3718))

(ppause :kick)
(ploop :kick {:instrument kick909})

(ppause :closed-hat)
(ploop :closed-hat {:instrument ch909
                    :note [:- 0]
                    :dur 1/2})

(ploop :clap {:instrument clap
              :note [:- 0]})

(stop)
(kick909)

(demo (sin-osc ))
(demo (lpf (var-saw :width 0) 600))

(definst fluflu [freq 440
                 gate 1
                 amp 1]
  (let [env (env-gen (adsr) :gate gate :action FREE)]
    (* amp
       env
       (lpf
        (var-saw freq #_(for [n (repeatedly 3 rand)]
                          (+ freq (* n 2))) :width 0.1)
        (* freq 6 env)))))

(u/keyboard-insts fluflu)
