(ns mad-sounds.sessions.aarschot-2026-02-02-squidcasashizzle
  (:require
   [overtone.live :refer :all]
   [vibeflow.session :as session]
   [vibeflow.util :as util]
   [casa.squid.jack :as jack]
   [casa.squid.midi :as midi]
   [casa.squid.midi.javax :as jxm]))

(defonce jack (jack/client :vibeflow))
(defonce midi-out (jack/midi-out-port jack :out))
(def midi-in (jack/midi-in-port jack :in))

(midi/add-receiver midi-in ::x (fn [msg offset]
                                 (println "Jack received" msg offset)))

(midi/add-receiver (jxm/find-input-device "Twister")
                   ::x
                   (fn [msg offset]
                     (println "Java received" msg offset)))
(midi/remove-receiver (jxm/find-input-device "Twister")
                      ::x)
(comment
  (map jxm/device-name (jxm/query-devices))

  (jxm/send
   (jxm/find-output-device "Snoop")
   (midi/message [0 :cc 1 0])
   0)

  (jxm/connect (jxm/find-input-device "Twister")
               (jxm/receiver-callback (fn [& args]
                                        (println '-> args))))

  (midi/-write midi-out (midi/message [0 :cc 1 124]) 0))
