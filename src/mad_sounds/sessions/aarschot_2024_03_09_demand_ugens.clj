(ns mad-sounds.sessions.aarschot-2024-03-09-demand-ugens
  (:require
   [overtone.live :refer :all]))

(def dorian (butlast (reductions + 0 (:dorian SCALE))))

(def b (doto (buffer (count dorian))
         (buffer-write! dorian)))

(defn rnd [min max]
  (+ (rand (- max min)) min))

(def ratios (repeatedly 6 #(rnd -0.1 0.1)))

(definst smooth-saw [idx 0
                     r0 (nth ratios 0)
                     r1 (nth ratios 1)
                     r2 (nth ratios 2)
                     r3 (nth ratios 3)
                     r4 (nth ratios 4)
                     r5 (nth ratios 5)
                     spread 1]
  (let [freq (midicps (+ 50 (degree-to-key b idx)))
        freq (* freq (midiratio [r0 r1 r2 r3 r4 r5]))]
    (moog-ff (splay (saw freq) :spread spread) 1500)))

(definst spread-saw [idx 0
                     spread 1
                     center 0
                     f-spread 0.05]
  (let [freq (midicps (+ 50 (degree-to-key b idx)))
        freq (* freq (midiratio (map #(* (- % 2) f-spread) (range 5))))]
    (moog-ff (splay (saw freq) :spread spread :center center) 1500)))

(def root-bus (control-bus))

(definst spread-saw [idx 0
                     spread 1
                     center 0
                     f-spread 0.05]
  (let [trig (impulse 4)
        degree (demand trig 0 (dseq [0 2 -1 -3] INF))
        freq (midicps (+ 50 (degree-to-key b (+ (in root-bus) degree))))
        freq (* freq (midiratio (map #(* (- % 2) f-spread) (range 5))))]
    (moog-ff (splay (saw freq) :spread spread :center center)
             (env-gen (envelope [600 2000 600] [1/16 1/4])
                      :gate trig))))
envelope
(demo (saw))

(bus)

(defsynth progression [root 0]
  (out root-bus 0))

(comment
  (smooth-saw)
  (spread-saw)
  (progression [:head 1])
  (dotimes [i 6]
    (ctl smooth-saw (str "r" i) (* 0.05 (- i 3))))

  (dotimes [i 6]
    (ctl smooth-saw (str "r" i) (rnd -0.25 0.25)))

  (ctl spread-saw :f-spread 0.10)
  (ctl spread-saw :spread 1)
  (kill progression)
  (stop))
