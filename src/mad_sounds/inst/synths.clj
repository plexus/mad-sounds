(ns mad-sounds.inst.synths
  (:require [overtone.live :refer :all]))

(definst plexus-kick [freq 120 dur 0.5 width 0.5]
    (let [freq-env (* freq (env-gen (perc 0 (* 0.99 dur))))
          env (env-gen (perc 0.01 dur) 1 1 0 1 FREE)
          sqr (* (env-gen (perc 0 0.01)) (pulse (* 2 freq) width))
          src (sin-osc freq-env)
          drum (+ sqr (* env src))]
      (compander drum drum 0.2 1 0.1 0.01 0.01)))

(definst beep [note 60 vol 1]
  (let
      [src (sin-osc (midicps note))
       env (env-gen (perc 0.01 0.3) :action FREE)]
    (* src env)))

(definst weirdos []
  (let [noise (lf-noise1 3)
        saws  (mul-add (lf-saw [5 5.123]) 3 80)
        freq  (midicps (mul-add noise 24 saws))
        src   (* 0.4 (sin-osc freq))]
    (comb-n src 1 0.3 2)))

(comment
  ((synth (out 0
               (let [note-f (mouse-x:kr 55 75)
                     note   (round note-f 2)
                     note2  (+ note 4)
                     ]
                 (sin-osc [(midicps note) (midicps note2)])))))

  ((synth (out 0
               (let [note-f (mouse-y:kr 55 75)
                     note   (+ 1 (round note-f 2))
                     note2  (+ note 8)
                     ]
                 (sin-osc [(midicps note) (midicps note2)]))))))
(stop)
(definst impulsar []
  (* 0.3
     (lpf
      (* (saw 440)
         (* (decay2 (impulse (mouse-x 1 200) 0))))
      (mouse-y 440 2000))))

(comment
 ((synth
   (out
    0
    (* (mouse-y)
       (sin-osc
        (+ 440
           (* 440 (mouse-x)))))))))
