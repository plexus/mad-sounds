(ns mad-sounds.sessions.aarschot-2024-03-16-bop
  (:require
   [overtone.live :refer :all]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; util

(defn rrand
  "Range rand, returns a random number between min (inclusive) and
  max (exclusive). Returns integers if both min and max are integer, floats
  otherwise."
  [min max]
  (+ min ((if (and (int? min) (int? max))
            rand-int
            rand)
          (- max min))))

(defn srand
  "Signed/symmetric rand, returns a value between -n and n."
  [n]
  (rrand (- n) n))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; patterns

(defn- pbind*
  "Internal helper for pbind, complicated by the fact that we want to cycle all
  seqs until we've fully consumed the longest seq, but we can't count because
  seqs could be infinite. So we track for which seqs we've reached the end at
  least once (done set)."
  [ks specs seqs done]
  (when-not (= (count done) (count ks))
    (let [vs (map (fn [v]
                    (if (sequential? v)
                      (first v)
                      v))
                  seqs)]
      (cons
       (zipmap ks vs)
       (lazy-seq
        (pbind*
         ks
         specs
         (map (fn [v spec]
                (if (sequential? v)
                  (let [n (next v)]
                    (if (nil? n)
                      spec
                      n))
                  v))
              seqs
              specs)
         (into done
               (remove nil?
                       (map (fn [k s]
                              (when (and (sequential? s)
                                         (not (next s)))
                                k))
                            ks
                            seqs)))))))))

(defn pbind
  "Takes a map, with some of the map values seqs. Returns a sequence of maps, with
  each successive map value constructed by taking the next value of each
  sequence. Sequences wrap (cycle) until the longest sequence has been consumed.
  Non-sequential values are retained as-is.

  Similar to SuperCollider's `PBind`, part of the Pattern library. "
  [m]
  (let [ks (keys m)
        specs (map m ks)
        seqs (map (fn [v]
                    (if (sequential? v)
                      (seq v)
                      v))
                  specs)
        done (set (remove nil?
                          (map (fn [k s]
                                 (when (not (sequential? s))
                                   k))
                               ks
                               seqs)))]
    (pbind*
     ks
     specs
     seqs
     done)))

(defn pwhite
  ([min max]
   (repeatedly #(rrand min max)))
  ([min max repeats]
   (repeatedly repeats #(rrand min max))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; events

(def default-event
  {:type             :note
   :mtranspose       0
   :gtranspose       0.0
   :ctranspose       0.0
   :octave           5.0
   :root             0.0
   :degree           0
   :scale            :major
   :steps-per-octave 12.0
   :detune           0.0
   :harmonic         1.0
   :octave-ratio     2.0
   :dur              1})

(declare derivations)

(defn kval [e k]
  (cond
    (contains? e k)
    (get e k)
    (contains? derivations k)
    ((get derivations k) e)
    :else
    (throw (ex-info (str "No derivation or event key " k)
                    {:event e}))))

(def derivations
  {:detuned-freq
   (fn [e]
     (+ (kval e :freq) (kval e :detune)))

   :freq
   (fn [e]
     (* (kval e :harmonic)
        (midi->hz
         (+ (kval e :midinote) (kval e :ctranspose)))))

   :midinote
   (fn [e]
     (+ 60
        (*
         (+ (kval e :octave) (- 5)
            (/ (+ (kval e :note)
                  (kval e :gtranspose)
                  (kval e :root))
               (kval e :steps-per-octave)))
         12 (/ (Math/log (kval e :octave-ratio))
               (Math/log 2)))))

   :scale-intervals
   (fn [e]
     (get SCALE (:scale e)))

   :scale-notes
   (fn [e]
     (butlast (reductions + 0 (kval e :scale-intervals))))

   :note
   (fn [e]
     (let [scale (kval e :scale-notes)
           size  (count scale)
           degree (+ (kval e :degree)
                     (kval e :mtranspose))]
       ;; Not too sure about this... would be good to compare results with SC
       (+
        (nth scale (mod degree size))
        (* (kval e :steps-per-octave)
           (cond-> (quot degree size)
             (< degree 0)
             dec)))))})

(def pname-mapping
  "If a synth has a :freq parameter, we actually use the computed :detuned-freq
  value."
  {:freq :detuned-freq})

(def playing (volatile! {}))

(defn invoke-inst [i freq gated? & args]
  (let [h (apply i args)]
    (prn :on i freq h)
    (when gated?
      (vswap! playing assoc-in [i freq] h))))

(defn handle-note-on [e]
  (let [i (:instrument e)
        params (:params i)
        args (reduce (fn [acc {:keys [name]}]
                       (let [kn (keyword name)
                             lk (get pname-mapping kn kn)]
                         (if (or (contains? e lk) (contains? derivations lk))
                           (conj acc kn (kval e lk))
                           acc)))
                     []
                     params)
        h (apply i args)]
    (if-let [t (:time e)]
      (apply-by t invoke-inst i (kval e :freq)
                (some #{"gate"} (map :name params))
                args)
      (apply invoke-inst i (kval e :freq)
             (some #{"gate"} (map :name params))
             args))))

(defn close-gate [h i freq]
  (prn :close h)
  (try
    (ctl h :gate 0)
    (vswap! playing update i dissoc freq)
    (catch Exception e
      (println "gate closed failed" freq)))
  )

(defn handle-note-off [e]
  (let [i (:instrument e)
        t (:time e)
        freq (kval e :freq)]
    (when-let [h (get-in @playing [i freq])]
      (if t
        (apply-by t close-gate [h i freq])
        (close-gate h i freq)))))

(on-event :note-on #'handle-note-on ::note-on)
(on-event :note-off #'handle-note-off ::note-off)

(def pplayers (volatile! {}))

(defn schedule-next [k]
  (vswap! pplayers
          (fn [pp]
            (let [{:keys [clock paused? pseq beat proto] :as player} (get pp k)
                  {:keys [dur type] :as e} (merge default-event (first pseq) proto)

                  next-seq (next pseq)
                  on-t     (clock beat)
                  off-t    (clock (+ beat dur))]
              (prn (:type e))
              (when (and player (not paused?))
                (if (= :note (:type e))
                  (do
                    (apply-by on-t event :note-on [(assoc e :time on-t)])
                    (apply-by off-t event :note-off [(assoc e :time off-t)]))
                  (apply-by event (:type e) [(assoc e :time on-t)])))
              (if next-seq
                (do
                  (apply-by off-t schedule-next [k])
                  (assoc-in pp [k :pseq] next-seq))
                (dissoc pp k))))))

(defn pplay [k clock pattern & {:as opts}]
  (vswap! pplayers assoc k (merge {:key     k
                                   :clock   clock
                                   :pattern pattern
                                   :pseq    pattern
                                   :beat    1
                                   :paused? false}
                                  opts))
  (schedule-next k))

(event :note-off :instrument mucklewain)
(stop)
(pplay :x (metronome 90)
       (pbind {:dur [0.5 1 0.5]
               :degree [1 2 3]})
       {:proto {:instrument mucklewain}})
(mucklewain)
(schedule-next :x)
@pplayers
@playing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcgen basic-splay
  [in-array    {:default [] :doc "List of input channels to splay."}
   spread      {:default 1 :doc "The audio spread width."}
   center      {:default 0 :doc "Center point of audio spread."}]
  (:ar (let [n (count in-array)]
         (mix (pan2 in-array
                    (for [i (range n)]
                      (+ center
                         (* spread
                            (- (* i
                                  (/ 2 (dec n)))
                               1)))))))))

(defcgen offsetify [freq {} n {} spread {}]
  (:ar
   (repeatedly n #(* freq (+ 1 (midiratio (srand spread)))))))

(definst mucklewain [freq 440 gate 1]
  (basic-splay
   (* (env-gen (adsr) :gate gate :action FREE)
      (rlpf (square (offsetify freq 4 0.1))
            (* 5 freq)
            0.1))
   (env-gen (envelope [1 0 1] [0.05 0.05]))))

(definst gloria [freq 265
                 gate 1.0
                 amp 0.5]
  (* (env-gen (adsr) :gate gate :action FREE)
     amp
     (+ (* 1.5 (sin-osc freq))
        (pluck (square) :delaytime (/ 1 (offsetify freq 9 0.6))))))


(gloria)

(doseq [e (take 50 (pbind {:instrument gloria
                           :degree (cons 0 (repeatedly 3 #(rrand 0 7)))
                           :scale :hindu
                           :dur (mapcat identity (repeatedly #(shuffle [100 200 300 400])))}))]
  (event :note-on e)
  (Thread/sleep (:dur e)))
