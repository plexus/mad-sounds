(ns mad-sounds.sessions.aarschot-2024dx4-stream-prep
  (:require
   [overtone.live :refer :all]
   [vibeflow.synths :as s]
   [vibeflow.util :as u]))

(s/churchbell)

(demo (sin-osc))

(event-debug-on)

(u/keyboard-insts s/churchbell)

(def deck1
  {:note [:g4 :f4 :e4 :d4 :c4 :d4 :e4 :c4]
   :dur [1.5 1/2 1 1 1 1 1 1]})

(def deck2
  {:note [:d :e :f :d :e :d :c :b :c]
   :octave [4 4 4 4 4 4 4 3 4]
   :dur [1/2 1/2 1/2 1/2 3/2 1/2 1 1 2]})

(def ode1
  {:note [:e :e :f :g :g :f :e :d :c :c :d :e :e :d :d]
   :octave 4
   :dur [1   1  1  1  1  1  1  1  1  1  1  1   3/2 1/2 1]}
  )

(pclear)

()

(pplay
 :x
 [(pbind ode1) (pbind deck2)]
 #_ode1
 #_[(pbind deck1) (pbind deck2)]
 :proto {:instrument s/marimba})
