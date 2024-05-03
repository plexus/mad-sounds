(ns mad-sounds.sessions.aarschot-2024-03-14-envelopes
  (:require
   [overtone.live :refer :all]))

(definst boop [note 40
               spread 1]
  (let [freq (midicps note)]
    (splay
     (* 2 (moog-ff
           (saw (for [i (range 5)]
                  (add-cents freq (* spread (- (rand-int 20) 10)))))
           (* 5 freq))))))

(definst boop [note 40
               spread 1]
  (let [freq (midicps note)]
    (splay
     (moog-ff
      (saw (for [i (range 5)]
             (add-cents freq (* spread (- (rand-int 20) 10)))))
      (* 5 freq)))))

(boop 45)
(ctl boop :spread 10)
(kill boop)
