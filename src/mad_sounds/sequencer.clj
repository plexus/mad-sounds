(ns mad-sounds.sequencer
  (:require [overtone.live :refer :all])
  (:require [mad-sounds.beat :as beat])
  (:require [mad-sounds.launchpad-mini :refer :all])
  (:require [mad-sounds.launchpad-leds :refer :all])
  (:require [launchpad.device :as device]))

(defonce sequences (atom (apply vector
                                (take 8 (repeat (apply vector
                                                       (take 8 (repeat 0))))))))

(defonce buffers (for [i (range 8)] (buffer 8)))


(defsynth minimal-sequencer
  [buf 0 sequencer 0 out-bus 0 rate 1]
  (let [cnt      (in:kr beat/beat-count-bus)
        beat-trg (in:kr beat/beat-bus)
        bar-trg  (and (buf-rd:kr 1 sequencer cnt) beat-trg)]
    (out out-bus (scaled-play-buf 1 buf rate bar-trg))))

(defn toggle [x y sequences]
  (assoc-in sequences [x y] (int (if (= (get-in sequences [x y]) 0) 1 0))))

(defn set-buffers! [sequences buffers]
  (doall (map (fn [s b] (buffer-write! b s)) sequences buffers)))

(defn set-leds! [sequences]
  (doall
   (for [x (range 8) y (range 8)]
     (do
       (if (= (int (get-in sequences [x y])) 1)
         (led-on launchpad-mini x y)
         (led-off launchpad-mini x y))))))

(defn print-grid []
  (doseq [row @sequences] (println row)) (println "---"))

(defn setup-handler []
  (handle-grid launchpad-mini
               (fn [note x y]
                 (swap! sequences (partial toggle x y))
                 (set-buffers! @sequences buffers)
                 (set-leds! @sequences)
                 (print-grid)
                 )
               "sequencer"))



(comment
  (use 'overtone.inst.drum)
  (use '[overtone.helpers.lib :only [uuid]])

  (use 'overtone.inst.sampled-piano)

  (handle-cell launchpad-mini 7 0 (fn [n x y] (sampled-piano (note :c3))) "piano")
  (handle-cell launchpad-mini 7 1 (fn [n x y] (sampled-piano (note :f3))) "piano")
  (handle-cell launchpad-mini 7 2 (fn [n x y] (sampled-piano (note :b3))) "piano")
  (handle-cell launchpad-mini 7 3 (fn [n x y] (sampled-piano (note :e4))) "piano")
  (handle-cell launchpad-mini 7 4 (fn [n x y] (sampled-piano (note :a4))) "piano")
  (handle-cell launchpad-mini 7 5 (fn [n x y] (sampled-piano (note :d5))) "piano")
  (handle-cell launchpad-mini 7 6 (fn [n x y] (sampled-piano (note :g5))) "piano")
  (handle-cell launchpad-mini 7 7 (fn [n x y] (sampled-piano (note :c6))) "piano")

  (handle-cell launchpad-mini 6 0 (fn [n x y] (sampled-piano (note :e3))) "piano")
  (handle-cell launchpad-mini 6 1 (fn [n x y] (sampled-piano (note :a3))) "piano")
  (handle-cell launchpad-mini 6 2 (fn [n x y] (sampled-piano (note :d4))) "piano")
  (handle-cell launchpad-mini 6 3 (fn [n x y] (sampled-piano (note :g4))) "piano")
  (handle-cell launchpad-mini 6 4 (fn [n x y] (sampled-piano (note :c5))) "piano")
  (handle-cell launchpad-mini 6 5 (fn [n x y] (sampled-piano (note :f5))) "piano")
  (handle-cell launchpad-mini 6 6 (fn [n x y] (sampled-piano (note :b5))) "piano")
  (handle-cell launchpad-mini 6 7 (fn [n x y] (sampled-piano (note :e6))) "piano")

  (kick-s)
  (snare)
  (def kicks (doall
              (for [x (range 8)]
                (mono-sequencer :buf kick-s :beat-num x :sequencer (nth buffers 0)))))



  (minimal-sequencer kick-s (nth buffers 0))
  ;(minimal-sequencer click-s (nth buffers 1))
  (minimal-sequencer snare (nth buffers 1))
  ;(minimal-sequencer kick-s (nth buffers 2))
  (minimal-sequencer subby-s (nth buffers 2))
  (minimal-sequencer sampled-piano (nth buffers 3))
(cymbal)
  (defonce kick-uuid (uuid))

  (kick-s)

  (buffer-write! (nth buffers 0) [1 0 1 0 1 0 1 0]) ;; kickq

  (on-trigger beat-trigger (fn [beat] (kick)) kick-uuid)


  (beat/start-the-beat! 140)
  (stop)
  (pp-node-tree)

  (defsynth mono-sequencer
    "Plays a single channel audio buffer."
    [buf 0 rate 1 out-bus 0 beat-num 0 sequencer 0 amp 1]
    (let [cnt      (in:kr beat/beat-count-bus)
          beat-trg (in:kr beat/beat-bus)
          bar-trg  (and (buf-rd:kr 1 sequencer cnt)
                        (= beat-num (mod cnt 8))
                        beat-trg)
          vol      (set-reset-ff bar-trg)]
      (out
       out-bus (* vol
                  amp
                  (pan2
                   (rlpf
                    (scaled-play-buf 1 buf rate bar-trg)
                    (demand bar-trg 0 (dbrown 200 20000 50 INF))
                    (lin-lin:kr (lf-tri:kr 0.01) -1 1 0.1 0.9))))))))
