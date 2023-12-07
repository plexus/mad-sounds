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
                [synth-key :off])))

(defn midi-ctl
  ([synth-key synth]
   (midi-ctl synth-key synth (into {}
                                   (map-indexed (fn [idx param]
                                                  [(+ 21 idx) (keyword (:name param))])
                                                (:params synth)))))
  ([synth-key synth mapping]
   (let [params     (:params (if (var? synth) @synth synth))
         midi-state (atom {})
         mapping    (into {} (map (juxt val key)) mapping)
         mapping    (into {} (keep (fn [param]
                                     (when-let [c (get mapping (keyword (:name param)))]
                                       [c param])))
                          params)]
     (prn mapping)
     (o/on-event [:midi :control-change]
                 (fn [{:keys [data1 data2]}]
                   (when-let [{:keys [name default min max step value]
                               :or {min 0}}
                              (get mapping data1)]
                     (let [synth (if (var? synth) @synth synth)
                           v (/ data2 127)
                           v (+ min (* (- max min) v)) ;; 0-127 -> min-max
                           v (if step
                               (* step (Math/round (double (/ v step))))
                               v)] ;; round to nearest step
                       (println name "=" (double v))
                       (reset! (:value (first (filter (comp #{name} :name) (:params synth)))) v)
                       (o/ctl synth (keyword name) v))))
                 [synth-key :ctl]))))

(defn capture-ctls [synth]
  (let [synth (if (var? synth) @synth synth)]
    (cons 'ctl
          (cons (symbol (:name synth))
                (apply concat
                       (for [{:keys [name default value]} (:params synth)
                             :when (not= default @value)]
                         [(keyword name) @value]))))))
