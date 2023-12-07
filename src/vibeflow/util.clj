(ns vibeflow.util
  (:require
   [overtone.core :as o]
   [casa.squid.jack :as jack]))

(defn overtone-ports []
  (filter #(re-find #"Overtone|SuperCollider" %)
          (jack/ports @jack/default-client #{:audio :out})))

(defn overtone-conns []
  (map vector
       (overtone-ports)
       (jack/ports @jack/default-client #{:audio :physical :in})))

(defn midi-preview [synth-key synth]
  (let [voices (atom {})]
    (o/on-event [:midi :note-on]
                (fn [{:keys [note]}]
                  (swap! voices assoc note (synth :note note)))
                [synth-key :on])
    (o/on-event [:midi :note-off]
                (fn [{:keys [note]}]
                  (o/ctl (get @voices note) :gate 0)
                  (swap! voices dissoc note))
                [synth-key :off]))
  (let [midi-state (atom {})
        mapping    (into {} (map-indexed (fn [idx p]
                                           [(+ 21 idx) p])) (:params synth))]
    (prn mapping)
    (o/on-event [:midi :control-change]
                (fn [{:keys [data1 data2]}]
                  (when-let [{:keys [name default min max step value]}
                             (get mapping data1)]
                    (prn name )
                    (let [v (/ data2 127)
                          v (+ min (* (- max min) v)) ;; 0-127 -> min-max
                          v (* step (Math/round (double (/ v step))))] ;; round to nearest step
                      (reset! value v)
                      (o/ctl synth (keyword name) v))))
                [synth-key :ctl])))
