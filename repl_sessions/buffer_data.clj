(ns repl-sessions.buffer-data
  (:require [overtone.live :refer :all]))

(def snare-fat (freesound 122053))
(reset! overtone.sc.machinery.server.comms/osc-debug* false)

(snd "/b_query" (:id snare-fat))

(snd "/b_getn"
     (:id snare-fat)
     0
     4000)

(recv )

(snd "/b_getn"
     (:id snare-fat)
     0
     100
     )

(into {} snare-fat)
