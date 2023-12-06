(ns mad-sounds.sessions.aarschot-2023-12-04-overtone-next
  (:require
   [overtone.live :refer :all]
   [vibeflow.util :as u]
   [casa.squid.jack :as jack]))

(jack/connect!
 (u/overtone-conns))

(use 'overtone.inst.synth)

(fm-demo)

(stop)
