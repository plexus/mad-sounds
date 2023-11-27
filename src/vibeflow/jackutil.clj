(ns vibeflow.jackutil
  (:require [vibeflow.midi.core :as midi]
            [vibeflow.midi.jack :as jack]))

(defn remove-cc [client]
  (let [in  (jack/midi-in-port client :remove-cc-in)
        out (jack/midi-out-port client :remove-cc-out)]
    (jack/register client
                   :process
                   ::remove-cc
                   (fn [_ _]
                     (jack/filter-pipe
                      in
                      out
                      (fn [msg]
                        (not= :cc (midi/event-type msg))))))
    client))

(defn monitor [client]
  (let [in (jack/midi-in-port client :monitor)]
    (jack/register client
                   :process
                   ::monitor
                   (fn [client frames]
                     (when-let [e (seq (jack/read-midi-events in))]
                       (run! (fn [[e t]]
                               (println t (midi/event e)))
                             e))
                     true)))
  client)

(defn jack-util [opts]
  (-> (jack/client "vibeflow")
      remove-cc
      monitor)
  @(promise))
