(ns mad-sounds.sessions.aarschot-2023-11-24-royal-road
  (:require
   [overtone.core :as o :refer :all]
   [overtone.music.pitch :as pitch]
   [casa.squid.jack :as jack]))

(connect-external-server)

(jack/connect!
 (map vector
      (filter #(re-find #"Overtone|SuperCollider" %)
              (jack/ports @jack/default-client #{:audio :out}))
      (jack/ports @jack/default-client #{:audio :physical :in})))

(jack/connections)


(require '[overtone.inst.piano :refer [piano]])
(require '[overtone.inst.sampled-piano :as p])

(invert-chord  (pitch/chord-degree :vi :e3 :major) -1)
(for [degree [:iv :v :iii :vi]]
  (cond-> (pitch/chord-degree degree :e3 :major)
    (= :vi degree)
    (invert-chord -2))
  )

(dotimes [_ 2]
  (doseq [degree [:iv :v :iii :vi]]
    (ctl p/sampled-piano :gate 0)
    (let [now (System/currentTimeMillis)
          pitches (pitch/chord-degree degree :c4 :major 5)]
      (p/sampled-piano :note (- (first pitches) 12))
      (p/sampled-piano :note (- (first pitches) 24))
      (doseq [note (shuffle (concat (invert-chord pitches (- (rand-int 4) 2))
                                    (reverse (invert-chord (butlast pitches) (- (rand-int 4) 2)))))
              :when (or (= note (first pitches))
                        (< 30 (rand-int 100)))]
        (p/sampled-piano :note note)
        (Thread/sleep (+ (rand-nth [180 180 180 180 180 360])
                         (rand-int 20))))
      (Thread/sleep (max (- 1800 (- (System/currentTimeMillis) now))
                         180)))
    #_(Thread/sleep (+ 1200 (rand-int 100)))))



(pitch/degree->interval :vi :major)
(pitch/degrees->pitches [:vi] :major :e1)
