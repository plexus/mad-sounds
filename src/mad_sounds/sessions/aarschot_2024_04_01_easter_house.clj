(ns mad-sounds.sessions.aarschot-2024-04-01-easter-house
  (:require
   [mad-sounds.euclid :refer :all]
   [overtone.core :refer :all]
   [overtone.studio.transport :as transport]))

(connect-server 55555)
(overtone.sc.machinery.server.connection/connect-jack-ports)

(require '[overtone.inst.drum :as drum]
         '[overtone.inst.synth :as synth]
         '[overtone.synth.stringed :as stringed])

(synth/simple-flute)
(stop)

(metro-bpm *clock* 120)
(defmacro pdo [& body]
  `(repeatedly (fn [] ~@body)))

(range 0 20 0.2)

(drum/soft-hat)
(stop)


(defn pseries
  ([start step]
   (iterate #(+ step %) start))
  ([start step size]
   (if (= Float/POSITIVE_INFINITY size)
     (pseries start step)
     (take size (pseries start step)))))

(defn pchoose [& args]
  (pdo (rand-nth args)))

(pplay :flute (pbind {:instrument synth/simple-flute
                      :degree (pdo (conj (rand-nth [#_[0 2 4]
                                                    #_[0 -2 3]
                                                    ;; [3 2 0]
                                                    ;; [-3 :rest -2 0]
                                                    ;; [6 2 4 0]
                                                    ;; [0 1 :rest 2]
                                                    ;; [4 3 2]
                                                    [6 7 :rest 0]
                                                    ;; [0 2 4]
                                                    ;; [0 4 2]
                                                    ;; [0 1 2]
                                                    ;; [:rest :rest :rest]
                                                    ;; [0 :rest 0 :rest]
                                                    ;; [6 2 6]
                                                    ;; [4 3 2]
                                                    ])
                                         :rest))
                      :amp 0.7
                      :octave [5]
                      :gtranspose
                      (concat (repeat 24 0)
                              (repeat 24 (- (degree->interval :iv :major))))
                      :mtranspose (for [m [0 3 5 6 4]]
                                    (repeat 6 m))
                      :dur 1/2 #_(pchoose 1 1 1 1/2 1/4) #_[1/4 1/4 1/3 1/3 1/3 1/4 1/2 1/2]}
                     INF))
(stop)
(pplay :kick
       (pbind
        {:instrument drum/kick
         :freq 90
         :dur 1}
        INF))

(pplay :hat
       (pbind
        {:instrument drum/soft-hat
         :freq 6000
         :amp [0.4 0.6]
         :dur [1/2]#_[(+ 1/4 1/8) (- 1/4 1/8)
                      (+ 1/4 1/16) (- 1/4 1/16)]}
        INF))

(ppause :hat)
(ppause :snare)
(presume :hat)
(presume :hat)
(pclear)
(pplay :snare
       (pbind
        {:instrument drum/snare
         :freq 400
         :dur [1 1/2 1 1/2 1]}
        INF)
       {:offset 0.5})

(zipmap [:a] (map pfirst [[1]]))

(pfirst [1 2])
(overtone.studio.event/schedule-next :snare)
(reset! overtone.studio.event/pplayers {})
(now)


(pplay :guitar
       (pbind {:type :ctl
               :instrument g
               :note-0 60
               :gate-0 [0 1]
               :note-1 60
               :gate-1 [1 0]
               }

              INF))
(stop)
(def g (stringed/guitar))
(:pnames (:sdef (into {} g)))
(stringed/pick-string [40 45 50 55 59 64] g 4 1 (now) )

(stringed/guitar)

(pclear)

(:params
 (into {} synth/simple-flute))
(:pnames
 (:sdef
  (into {} synth/simple-flute)))
