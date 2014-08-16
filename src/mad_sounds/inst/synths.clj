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

(definst pw-gong [frequency 80 duration 2]
  "Another port of Paul Weineke's gong.
original (CLM): https://ccrma.stanford.edu/software/snd/snd/clm-ins.scm
c-sound: http://varioussmallfires.blogspot.com/2011/07/fm-gong.html
supercollider: http://varioussmallfires.blogspot.com/2012/01/supercollider-gong.html"
  (let [mod1f (* frequency 1.16)
        mod2f (* frequency 3.14)
        mod3f (* frequency 1.005)

        idx1a (* 0.01 mod1f)
        idx1b (* 0.30 mod1f)
        idx1scaler (- idx1b idx1a)

        idx2a (* 0.01 mod2f)
        idx2b (* 0.38 mod2f)
        idx2scaler   (- idx2b idx2a)

        idx3a (* 0.01 mod3f)
        idx3b (* 0.50 mod3f)
        idx3scaler   (- idx3b idx3a)

        scaled-env (fn [levels durations] (envelope levels (map #(* % duration) durations)))

        fmup   (env-gen (scaled-env [0 1 1 0]       [0.75 0.24 0.01]))
        fmdown (env-gen (scaled-env [0 1 0]         [0.02 0.98]))
        rise   (env-gen (scaled-env [0 0.3 1 0.5 0] [0.15 0.15 0.45 0.25]))
        ampenv (env-gen (envelope   [0 1 0.001]     [0.002 (- duration 0.002)] :exponential))

        mod1 (* (+ idx1a idx1scaler) fmup   (sin-osc mod1f))
        mod2 (* (+ idx2a idx2scaler) fmdown (sin-osc mod2f))
        mod3 (* (+ idx3a idx3scaler) rise   (sin-osc mod3f))]
    (g-verb
     :roomsize 5
     :damping 0.7
     :in (* ampenv
            (sin-osc (+ frequency mod1 mod2 mod3))))))

(comment
  "Attempt to make the pw-gong sustain and respond to a gate signal, not working so far"

  (definst pw-gong-gated [frequency 80 duration 2 gate 1]
    "Another port of Paul Weineke's gong.
original (CLM): https://ccrma.stanford.edu/software/snd/snd/clm-ins.scm
c-sound: http://varioussmallfires.blogspot.com/2011/07/fm-gong.html
supercollider: http://varioussmallfires.blogspot.com/2012/01/supercollider-gong.html"
    (let [mod1f (* frequency 1.16)
          mod2f (* frequency 3.14)
          mod3f (* frequency 1.005)

          idx1a (* 0.01 mod1f)
          idx1b (* 0.30 mod1f)
          idx1scaler (- idx1b idx1a)

          idx2a (* 0.01 mod2f)
          idx2b (* 0.38 mod2f)
          idx2scaler   (- idx2b idx2a)

          idx3a (* 0.01 mod3f)
          idx3b (* 0.50 mod3f)
          idx3scaler   (- idx3b idx3a)

          scaled-env (fn [levels durations & {:keys [curve] :or {curve :linear}}]
                       (env-gen (envelope
                                 levels
                                 (map #(* % duration) durations)
                                 curve
                                 (count durations))
                                gate))

          fmup   (scaled-env [0 1 1 0]       [0.75 0.24 0.01])
          fmdown (scaled-env [0 1 0]         [0.02 0.98])
          rise   (scaled-env [0 0.3 1 0.5 0] [0.15 0.15 0.45 0.25])
          ampenv (scaled-env [0 1 0] [0.002 (- duration 0.002)] :curve :exponential)

          mod1 (* (+ idx1a idx1scaler) fmup   (sin-osc mod1f))
          mod2 (* (+ idx2a idx2scaler) fmdown (sin-osc mod2f))
          mod3 (* (+ idx3a idx3scaler) rise   (sin-osc mod3f))]
      (g-verb
       :roomsize 5
       :damping 0.7
       :in (* ampenv
              (sin-osc (+ frequency mod1 mod2 mod3)))))))

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
