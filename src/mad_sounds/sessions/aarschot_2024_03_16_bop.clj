(ns mad-sounds.sessions.aarschot-2024-03-16-bop
  (:require
   [overtone.live :refer :all]))

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
   :octave-ratio     2.0})

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

(defn play-event [e]
  (let [e (merge default-event e)
        i (:instrument e)
        args (reduce (fn [acc {:keys [name]}]
                       (let [kn (keyword name)
                             lk (get pname-mapping kn kn)]
                         (if (or (contains? e lk) (contains? derivations lk))
                           (conj acc kn (kval e lk))
                           acc)))
                     []
                     (:params i))]
    (apply i args)))

(defn pbind* [ks specs seqs done]
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

(defn pbind [m]
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

(pbind {:foo [1 2]
        :bar [3]
        :baz 4
        :type :note
        })

(next ())

(on-event :note play-event ::play-event)

(play-event {:instrument mucklewain
             :degree 0
             }
            )

(defn rrand [min max]
  (+ min (rand (- max min))))

(defn srand [n]
  (rrand (- n) n))

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

(defsynth mucklewain [freq 440 gate 1]
  (out 0
       (basic-splay
        (* (env-gen (perc) :gate gate)
           (rlpf (square (offsetify freq 4 0.1))
                 (* 5 freq)
                 0.1))
        (env-gen (envelope [1 0 1] [0.05 0.05])))))

(event :note :instrument mucklewain)



(definst gloria [note 30]
  (let [f (midicps note)]
    (* (env-gen (perc :release 7.5))
       (+ (* 1.5 (sin-osc f))
          (pluck (square) :delaytime (/ 1 (offsetify f 9 0.6)))))))

(defsynth beat []
  (out:kr trig-bus (impulse 90)))

(beat [:tail 1])
(mucklewain [:tail 1] 40)

(gloria 32)
(kill mucklewain)
(kill mucklewain)
(stop)
(demo)
