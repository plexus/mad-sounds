(ns mad-sounds.instruments
  (:require [overtone.live :refer :all]))

(definst steel-drum [note 60 amp 0.8]
  (let [freq (midicps note)]
    (* amp
       (env-gen (perc 0.01 0.2) 1 1 0 1 :action FREE)
       (+ (sin-osc (/ freq 2))
          (rlpf (saw freq) (* 1.1 freq) 0.4)))))



;; Simple additive synth, combining the first seven harmonics of a base frequency

(definst additive [freq 440 h1 1 h2 0 h3 0 h4 0 h5 0 h6 0 h7 0 h8 0 h9 0]
  (let [amplitudes  [h1 h2 h3 h4 h5 h6 h7 h8 h9]
        mk-harmonic (fn [idx amp]
                      (* (sin-osc (* freq (inc idx))) amp))]
    (normalizer (mix (map-indexed mk-harmonic amplitudes)))))

;; Approximation of a sawtooth wave, harmonics have amplitude 1/n

(doall
 (doseq [x (range 3) y (range 2)] (println [x y])))

(defn set-harmonics [& rest]
  (doseq [[n h] (map list (map inc (range)) rest)]
    (ctl additive (keyword (str "h" n)) h)))

;; Approximation of a saw tooth wave, all harmonics are present with amplitude 1/n

(do
 (additive)
 (set-harmonics 1 1/2 1/3 1/4 1/5 1/6 1/7 1/8 1/9))

;; Approximation of a triangle wave, every odd harmonic has amplitude 1/n^2

(do
 (additive)
 (set-harmonics 1
                0
                (/ 1 (* 3 3))
                0
                (/ 1 (* 5 5))
                0
                (/ 1 (* 7 7))
                0
                (/ 1 (* 9 9))))


;; Approximation of a square wave, every odd harmonic has amplitude 1/n

(do
 (additive)
 (set-harmonics 1 0 1/3 0 1/5 0 1/7 0 1/9))
(stop)

(* 440 9)



(do
  (definst plain-saw [freq 440]
    (saw freq))
  (plain-saw))

(do
  (definst plain-square [freq 440]
    (square freq))
  (plain-square))

(do
  (definst plain-square [freq 440]
    (square freq))
  (plain-square))
(stop)

(do
  (definst filtered-square [freq 440 cutoff (* 9 440)]
    (lpf (square freq) cutoff))
  (filtered-square))

(do
  (definst filtered-square-mouse [freq 440 cutoff (* 15 440)]
    (normalizer (lpf (* (square freq) (lf-tri:kr (+ 10 (* 5 (mouse-x))))) (mouse-y freq cutoff))))
  (filtered-square-mouse))

(stop)
(do
  (definst plain-triangle [freq 440]
    (lf-tri freq))
  (plain-triangle))


(do
  (definst modulation-test
    "Take a basic sine wave, but modulate the frequency with a low frequency sine wave"
    [freq 440 rate 0.5 depth 10]
    (sin-osc (* freq (+ 1 (/ (sin-osc:kr rate) depth)))))
  (modulation-test))


(do
  (definst envelope-test
    [freq 440 level 1 rate 1 loop? 0 attack 0.1 decay 0.3 sustain 0.2 release 0.2 curve -4 gate 1]
    (let [adsr-curve (adsr attack decay sustain release level curve)
          env (env-gen adsr-curve :gate gate :action FREE)
          env2 (env-gen (perc) :gate gate :action FREE)
          snd (* (saw freq) (sin-osc:kr 15))]
      (* snd env2)))
  (envelope-test)
  (ctl envelope-test :gate 0)
)

(defsynth foo [freq 440]
  (let [env (env-gen (perc 0.1 3.5) :action FREE)
        snd (saw  [freq (* freq 1.01)])
        snd (normalizer (lpf snd (mouse-y 300 10000)))]
    (out 0 (pan2 (* snd env)))))

(foo)

(ctl modulation-test :depth 50)

(stop)
