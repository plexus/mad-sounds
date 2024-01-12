(ns mad-sounds.sessions.aarschot-2024-01-07-bassline
  (:require
   [overtone.live :refer :all]
   [vibeflow.util :as util]))

(demo
  10
  (pan2
   (pluck (white-noise)
          :delaytime 1/100
          :decaytime 100
          :coef 0.6)))


(definst bell [note 60
               gate 1]
  (let [freq (midicps note)
        decaytime 100
        burst (var-saw freq)
        filter-coef 0.5
        filter-coef2 0.6]
    (* (env-gen (adsr) :gate gate :action FREE)
       (rlpf
        (mix
         [(pluck burst
                 :delaytime (/ 1 freq)
                 :decaytime decaytime
                 :coef filter-coef)
          (* (line 0 1 6)
             (pluck burst
                    :delaytime (/ 1 (+ freq 2))
                    :decaytime decaytime
                    :coef filter-coef2))
          (* (line 0.1 0.6 6)
             (pluck burst
                    :delaytime (/ 1 (+ freq 10))
                    :decaytime decaytime
                    :coef filter-coef2))
          (* (line 0 1 4)
             (pluck burst
                    :delaytime (/ 1 (- freq 1))
                    :decaytime decaytime
                    :coef filter-coef2))])
        (* 3 freq)#_(line (* 3 freq) (* 5 freq)  )))))


(definst bell []
  (let [freq (midicps 65)
        decaytime 6
        burst (var-saw freq)
        filter-coef 0.3
        filter-coef2 0.4]
    (rlpf
     :in (+
          (pluck burst
                 :delaytime (/ 1 freq)
                 :decaytime decaytime
                 :coef filter-coef)
          (* (line:kr 0.1 0.3 4)
             (pluck burst
                    :delaytime (/ 1 (* 1.01 freq))
                    :decaytime decaytime
                    :coef filter-coef2))
          (* 0.3
             (pluck burst
                    :delaytime (/ 1 (* 2.15 freq))
                    :decaytime decaytime
                    :coef filter-coef2)))
     :freq (line (* 5 freq) (* 1.3 freq) 1.5))))

(util/defloop churchbell 1 [m b]
  (at (m b) (bell)))

(churchbell (metronome 30))
(defn churchbell [ _])
(stop)
(stop)
(util/midi-preview :bell #'bell)
(util/midi-preview-off :keys)
(stop)

(util/spectro)

(line )

(demo (sin-osc 440))
(stop)
