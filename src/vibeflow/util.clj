(ns vibeflow.util
  (:require
   [casa.squid.jack :as jack]))

(defn overtone-ports []
  (filter #(re-find #"Overtone|SuperCollider" %)
          (jack/ports @jack/default-client #{:audio :out})))

(defn overtone-conns []
  (map vector
       (overtone-ports)
       (jack/ports @jack/default-client #{:audio :physical :in})))
