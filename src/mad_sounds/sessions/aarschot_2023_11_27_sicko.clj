(ns mad-sounds.sessions.aarschot-2023-11-27-sicko
  (:require
   [casa.squid.jack :as jack]
   [overtone.at-at :as at]
   [vibeflow.util :as util]
   [overtone.live :refer :all]
   [overtone.studio.midi :as midi]))

(jack/ports)
(jack/connections)
(jack/connect!
 (concat
  (util/overtone-conns)
  (map vector
       (util/overtone-ports)
       (filter #(re-find #"jaaa" %)
               (jack/ports @jack/default-client #{:audio :in})))))

;;;;;


(definst fm [freq 440 ratio 3 depth 300]
  (let [mod (sin-osc :freq (* freq (floor ratio)))
        car (sin-osc :freq (+ freq (* mod depth)))
        env (env-gen (perc 0.2 0.5 0.7 :linear) :action FREE)]
    (* car env)))
(fm)
(def pool (at/mk-pool))

(at/every 1000 #(fm) pool)
(run! at/kill (at/scheduled-jobs pool))
(stop)

(def midi-state (atom {}))

(midi-inst-controller
 midi-state
 (partial ctl fm)
 {21 [:ratio     #(* 10 (/ % 127.0))]
  22 [:depth      #(* 1000 (/ % 127.0))]
  })
