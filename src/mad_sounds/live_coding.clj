(ns mad-sounds.live_coding
  (:use overtone.live))

(definst kick [freq 120 dur 0.5 width 0.5]
  (let [freq-env (* freq (env-gen (perc 0 (* 0.99 dur))))
        env (env-gen (perc 0.01 dur) 1 1 0 1 FREE)
        sqr (* (env-gen (perc 0 0.01)) (pulse (* 2 freq) width))
        src (sin-osc freq-env)
        drum (+ sqr (* env src))]
    (compander drum drum 0.2 1 0.1 0.01 0.01)))

;(kick)

(definst c-hat [amp 0.8 t 0.04]
  (let [env (env-gen (perc 0.001 t) 1 1 0 1 FREE)
        noise (white-noise)
        sqr (* (env-gen (perc 0.01 0.04)) (pulse 880 0.2))
        filt (bpf (+ sqr noise) 9000 0.5)]
    (* amp env filt)))

;(c-hat)


(def metro (metronome 128))

;(metro) ; => current beat number
;(metro 100) ; => timestamp of 100th beat

(defn player [beat]
  (at (metro beat) (kick))
  (at (metro (+ 0.5 beat)) (c-hat))
  (apply-at (metro (inc beat)) #'player (inc beat) []))

(player (metro))
;(stop)
(metro-bpm metro 110)

; http://www.youtube.com/watch?v=imoWGsipe4k

; https://github.com/overtone/overtone/blob/master/docs/sc-book/sc-one.clj

(demo 10 (sin-osc (+ 1000 (* 600 (lf-noise0:kr 12))) 0.3))
(demo 10 (rlpf (dust [12 15]) (+ 1600 (* 1500 (lf-noise1 [1/3, 1/4]))) 0.02 ))

(demo 2 (let [sines 5
              speed 6]
           (* (mix
               (map #(pan2 (* (sin-osc (* % 100))
                              (max 0 (+ (lf-noise1:kr speed) (line:kr 1 -1 30))))
                           (- (clojure.core/rand 2) 1))
                    (range sines)))
              (/ 1 sines))))


(definst foo []
  (let [noise (lf-noise1 3)
               saws  (mul-add (lf-saw [5 5.123]) 3 80)
               freq  (midicps (mul-add noise 24 saws))
               src   (* 0.4 (sin-osc freq))]
           (comb-n src 1 0.3 2)))
;; (foo)
;; (stop)

;; danielaramos27@yahoo.com.ar
;; agpichersky@hotmail.com
