(ns vibeflow.util
  (:require
   [casa.squid.jack :as jack]
   [clojure.java.shell :as sh]
   [overtone.core :as o]))

(def special-ctls #{"gate" "note" "tone"})

(defn overtone-ports []
  (filter #(re-find #"Overtone|SuperCollider" %)
          (jack/ports @jack/default-client #{:audio :out})))

(defn overtone-conns []
  (map vector
       (overtone-ports)
       (jack/ports @jack/default-client #{:audio :physical :in})))

(defn midi-preview
  ([synth-key synth]
   (midi-preview nil synth-key synth))
  ([ch synth-key synth]
   (let [voices (atom {})]
     (o/on-event [:midi :note-on]
                 (fn [{:keys [note channel]}]
                   (when (or (nil? ch) (= ch channel))
                     (swap! voices assoc note (synth :note note))))
                 [synth-key :on])
     (o/on-event [:midi :note-off]
                 (fn [{:keys [note channel]}]
                   (when (or (nil? ch) (= ch channel))
                     (o/ctl (get @voices note) :gate 0)
                     (swap! voices dissoc note)))
                 [synth-key :off]))))

(defn midi-preview-off [synth-key]
  (o/remove-event-handler [synth-key :on])
  (o/remove-event-handler [synth-key :off]))

(defn pad->note [p]
  (cond
    (< 0 p 5) (+ p 39)
    (< 4 p 9) (+ p 43)
    (< 8 p 13) (+ p 27)
    (< 12 p 17) (+ p 31)))

(defn midi-pad
  ([ch pad synth-key synth]
   (o/on-event [:midi :note-on]
               (fn [{:keys [note channel]}]
                 (when (and (= note (pad->note pad))
                            (or (nil? ch) (= ch channel)))
                   (synth)))
               [synth-key :pad])))

(defn param [inst pname]
  (some #(when (= (name pname) (:name %)) %)
        (:params inst)))

(defn ctl! [inst & args]
  (doseq [[pname v] (partition 2 args)]
    (reset! (:value (param inst pname)) v)
    (o/ctl inst (keyword pname) v)))

(defn midi-ctl
  ([synth-key synth]
   (let [params (:params (if (var? synth) @synth synth))]
     (midi-ctl synth-key synth (into {}
                                     (map-indexed (fn [idx param]
                                                    [(+ 21 idx) (keyword (:name param))])
                                                  (remove (comp special-ctls :name)
                                                          params))))))
  ([synth-key synth mapping]
   (let [params     (:params (if (var? synth) @synth synth))
         midi-state (atom {})
         mapping    (into {} (map (juxt val key)) mapping)
         mapping    (into {} (keep (fn [param]
                                     (when-let [c (get mapping (keyword (:name param)))]
                                       [c param])))
                          params)]
     (o/on-event [:midi :control-change]
                 (fn [{:keys [data1 data2]}]
                   (println "CC" data1 data2)
                   (println mapping)
                   (println            (get mapping data1))

                   (when-let [{:keys [name default min max step value]
                               :or {min 0}}
                              (get mapping data1)]
                     (let [synth (if (var? synth) @synth synth)
                           v (if (and min max) (/ data2 127) data2)
                           v (if (and min max) (+ min (* (- max min) v)) v) ;; 0-127 -> min-max
                           v (if step
                               (* step (Math/round (double (/ v step))))
                               v)] ;; round to nearest step
                       ;;(println name "=" (double v))
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

(defn spectro []
  (future
    (sh/sh "pw-jack" "wolf-spectrum"))
  (future
    (loop []
      (if (not (some #{"Wolf Spectrum:in1"} (jack/ports)))
        (do
          (Thread/sleep 500)
          (recur))
        (doseq [[from to] [["SuperCollider:out_1" "Wolf Spectrum:in1"]
                           ["SuperCollider:out_2" "Wolf Spectrum:in2"]
                           ["Overtone:out_1" "Wolf Spectrum:in1"]
                           ["Overtone:out_2" "Wolf Spectrum:in2"]]]
          (try
            (jack/connect from to)
            (catch Exception _)))))))

(defn round-to-multiple-down [i m]
  (long (* m (Math/floor (/ i m)))))

(defn round-to-multiple-up [i m]
  (long (* m (Math/ceil (/ i m)))))

(defmacro defloop [lname beats [metro-sym beat-sym] & body]
  `(def
     ;; jumping through some hoops here to make sure both the var and the fn can
     ;; be passed to `kill`
     ~(with-meta lname
        `{'overtone.sc.protocols/kill*
          (fn [v#]
            (alter-meta! v# assoc :killed true))})
     ~(with-meta
        `(fn
           ([metro#]
            (~lname metro# (round-to-multiple-down (metro#) ~beats)))
           ([metro# beat#]
            (if (:killed (meta (var ~lname)))
              (alter-meta! (var ~lname) dissoc :killed)
              (let [~metro-sym metro#
                    ~beat-sym beat#
                    next-beat# (+ beat# ~beats)]
                ~@body
                (o/apply-by (metro# next-beat#) (var ~lname) [metro# next-beat#])))))
        `{'overtone.sc.protocols/kill*
          (fn [v#]
            (alter-meta! (var ~lname) assoc :killed true))})
     ))

(defn keyboard-insts [& insts]
  (o/on-event [:midi :note-on]
              (fn [{:keys [note channel velocity] :as e}]
                (when-let [inst (get (vec insts) channel)]
                  (o/event :note :instrument inst :midinote note
                           :id [inst note]
                           :end-time nil
                           :amp (* 1.5 (/ velocity 128) @(:value (param inst "amp"))))))
              ::midi-on)

  (o/on-event [:midi :note-off]
              (fn [{:keys [note channel] :as e}]
                (println :NOTE_OFF)
                (when-let [inst (get (vec insts) channel)]
                  (println "GOT INST" inst)
                  #_(o/ctl inst :gate 0)
                  (o/event :note-end :instrument inst :midinote note
                           :id [inst note]
                           )))
              ::midi-off))
