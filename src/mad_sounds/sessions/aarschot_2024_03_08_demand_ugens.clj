(ns mad-sounds.sessions.aarschot-2024-03-08-demand-ugens
  (:require
   [overtone.live :refer :all]))

(demo (moog-ff (splay (saw (* (midicps 50)
                              (repeatedly 4 #(midiratio (- (rand 0.4) 0.2)))))) 1500))

(def dorian (butlast (reductions + 0 (:dorian SCALE))))

(def b (doto (buffer (count dorian))
         (buffer-write! dorian)))

(definst smooth-saw [idx 0]
  (let [freq (midicps (+ 50 (degree-to-key b idx)))
        freq (* freq (repeatedly 4 #(midiratio (- (rand 0.2) 0.1))))]
    (moog-ff (splay (saw freq)) 1500)))

(dotimes [i 8]
  (smooth-saw i)
  (Thread/sleep 300)
  (kill smooth-saw))

(definst smooth-saw2 []
  (let [trig (impulse (mouse-x 2 150 1))
        deg  (demand trig 0 (dseq [0 2 -1 -3] INF))
        freq (midicps (+ 50 (degree-to-key b deg)))
        freq (* freq (repeatedly 4 #(midiratio (- (rand 0.2) 0.1))))]
    (moog-ff (splay (saw freq)) (mouse-y 100 8000 1))))

(definst smooth-saw3 []
  (let [trig (impulse (mouse-x 2 150 1))
        deg  (duty (drand [1/4 1/8] INF) 0 (dseq [0 2 -1 -3] INF))
        freq (midicps (+ 50 (degree-to-key b deg)))
        freq (* freq (repeatedly 4 #(midiratio (- (rand 0.2) 0.1))))]
    (moog-ff (splay (saw freq)) (mouse-y 100 8000 1))))

(smooth-saw3)

(stop)
