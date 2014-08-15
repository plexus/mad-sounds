(ns mad-sounds.scratch
  (:require [overtone.live :refer :all]
            [overtone.inst.synth :refer :all]
            [mad-sounds.inst.sampled :refer :all]
            [mad-sounds.inst.synths :refer :all]))


(def *base* (note :C1))
(def *scale* (partial nth-interval :major))

(defn scale-nth [idx]
  (+ *base* (*scale* idx)))

(def *sig* 4)
(def *drums* [[kick-fat  [0 2]]
              [snare-fat [1 3]]
              [click-s   [1/2   3 7/2]]])

(def *bass-line* (map scale-nth [0 2 0 2  6 3 5 4  0 0 0 0  0 5 4 3]))

(defn from [metro offset]
  (fn [beat] (metro (+ beat offset))))

(defn speed-up [metro factor]
  (fn [beat] (metro (/ beat factor))))

(defn -rewrite-ats [timervar]
  (fn [forms]
    (for [form forms]
      (if (list? form)
        ((-rewrite-ats timervar)
         (if (= (first form) 'at)
           (seq (assoc-in (vec form) [1] `(~timervar ~(nth form 1))))
           form))
        form))))

(defmacro defloop [name count args & body]
  (let [timervar 'timer#
        newbody (map (-rewrite-ats timervar) body)]
    `(defn ~name [~timervar ~@args]
       ~@newbody
       (apply-by (~timervar ~next) #'~name [(from ~timervar ~count) ~@args]))))


(defn even-melody [timer inst [note & notes]]
  (do
    (at (timer 0) (let [i (inst note)]
                      (at (timer 1) (ctl i :gate 0))))
    (let [next (from timer 0.5)]
      (if notes
        (even-melody next inst notes)
        next))))

;; (defn loop-melody [timer inst notes]
;;   (let [next (even-melody timer inst notes)]
;;     (apply-by (next 0)
;;      #'loop-melody next inst notes [])))


(defn loop-bass [timer inst]
  (let [next (even-melody timer inst *bass-line*)]
    (apply-by (next 0) #'loop-bass next inst [])))

(defloop beat-loop *sig* []
  (doseq [[drum pattern] *drums*]
    (doseq [time pattern] (at time (drum)))))

(defn loop-beat [timer]
  (doseq [[drum pattern] *drums*]
    (doseq [time pattern]
      (at (timer time) (drum))))
  (apply-by (timer *sig*) #'loop-beat [(from timer *sig*)]))

(defn play! [m]
  (loop-bass m vintage-bass)
  (loop-beat m))

(stop)
(let [m (metronome 120)]
  (loop-bass m #(cs80lead (midi->hz %))))

(let [m (metronome 120)]
  (loop-bass m #(tb303 (midi->hz %) :amp 1)))

(volume 0.8)
(play! (metronome 120))

(defn from [metro offset]
  (fn [beat] (metro (+ beat offset))))

(defn kick-and-snare [metro]
  (at (metro 1) (kick-fat))
  (at (metro 2) (snare-fat))
  (apply-by (metro 3) #'kick-and-snare [(from metro 2)]))


(def *m* (metronome 120))

(*m* 1)

(loop-beat (from *m* (*m*)))
(loop-bass (from *m* (*m*)) vintage-bass)

(loop-bass (metronome 120) vintage-bass)

(stop)

(resolve-scale :ionian)
(choose (scale-field :g :ionian))

(stop)
(stop)
(kick-fat)
(snare-fat)

(click-s)
(boom-s)
(flute-A)
(volume 0.3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Session with til at eurucamp

(defonce impulse-bus (control-bus))
(defonce count-bus   (control-bus))
(defonce beat-trigger-id (trig-id))

(def base-impulse (atom nil))

(defsynth base-impulse-generator [rate (/ 90.0 60)]
  (out:kr impulse-bus (impulse:kr rate)))

(defsynth impulse-counter []
  (out:kr count-bus (pulse-count (in:kr impulse-bus))))

(defsynth count->trigger []
  (send-trig
   :in (in:kr impulse-bus)
   :id beat-trigger-id
   :value (in:kr count-bus)))


(defn start-the-beat! []
  (swap! base-impulse (fn [_] (base-impulse-generator)))
  (impulse-counter [:after @base-impulse])
  (count->trigger))

(defn play-pattern [name patterns]
  (on-trigger beat-trigger-id
              (fn [beat]
                (doseq [inst (keys patterns)]
                  (let [pattern (patterns inst)]
                    (if (beat-match? ()))
                    )
                  )
                ) name))

(play-pattern 'my-pattern'
              {
               snare-fat [ 0 0 0 1 0 1 0 1 ]
               kick-fat  [ 1 0 1 0 1 0 1 0 ]
               }

              )

(defn beat-match? [beat pos]
  (= (int (mod beat 8)) pos))

(on-trigger beat-trigger-id (fn [_] ) "trigger-key")
(on-trigger beat-trigger-id (fn [_] (snare-fat)) "trigger-key-2")

(on-trigger beat-trigger-id
            (fn [x]
              (if (= (int (mod x 2)) 0)
                (kick-fat)
                (snare-fat))) "trigger-key")

(start-the-beat!)
(ctl @base-impulse :rate (/ 150.0 60))
(volume 4)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(comment
  "Original instrument.clj, experiments with additive synths"

  (scope)


  (do
    (definst super-simple-synth [freq 440]
      (+
       (sin-osc freq)
       (/ (sin-osc (* freq 2)) 2)))

    (super-simple-synth)

    )

  (stop)

  (lp-bind-octave super-simple-synth 7)

  (definst additive-1 [freq 440])

  (midi->hz (note :A4))

  (volume 0.5)
  (stop)



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
    (set-harmonics 1 0   0   0   0   0   0   0   0))

  (set-harmonics 1 1/2 1/3 1/4 1/5 1/6 1/7 1/8 1/9)

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
  (stop)

  (volume 0.2)

  (do
    (definst plain-square [freq 440]
      (square freq))
    (plain-square))
  (stop)

  (use 'mad-sounds.launchpad-mini)

  (lp-bind-octave plain-square 0)
  (stop)

  (do
    (definst plain-square [freq 440]
      (square freq))
    (plain-square))

  (do
    (definst filtered-square [freq 440 cutoff (* 9 440)]
      (lpf (square freq) cutoff))
    (filtered-square))

  (do
    (definst filtered-square-mouse [freq 440 cutoff (* 15 440)]
      (normalizer
       (lpf
        (*
         (square freq)
         (lf-tri:kr (+ 10 (* 5 (mouse-x)))))
        (mouse-y freq cutoff))))
    (filtered-square-mouse))
  (noise)
  (stop)
  (do
    (definst plain-triangle [freq 440]
      (lf-tri freq))
    (plain-triangle))

  (do
    (stop)
    (definst out-of-phase-saws [freq 440 gate 1]
      (* (env-gen (adsr) gate)
         (mix [(saw (+ (* 2 (lf-tri 19)) freq)) (* (/ (sin-osc 15) 1) (saw (+ 3 (* 2 (lf-tri 5)) freq)))])))
    (out-of-phase-saws))

  (out-of-phase-saws (midi->hz (note :c4)))
  (out-of-phase-saws (midi->hz (note :e4)))
  (out-of-phase-saws (midi->hz (note :g4)))

  (ctl out-of-phase-saws :gate 0)

  (lp-bind-octave out-of-phase-saws 1)




  (do
    (definst modulation-test
      "Take a basic sine wave, but modulate the frequency with a low frequency sine wave"
      [freq 440 rate 0.5 depth 10]
      (sin-osc (* freq (+ 1 (/ (sin-osc:kr rate) depth)))))
    (modulation-test))

  (stop)


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

  (defsynth foo [freq 440 gate 1]
    (let [env (env-gen (perc 0.1 3.5) :action FREE :gate gate)
          snd (saw  [freq (* freq 1.01)])
          snd (normalizer (lpf snd (mouse-y 300 10000)))]
      (out 0 (pan2 (* snd env)))))

  (definst distorter [freq 880
                      attack 0.4
                      decay 0.5
                      sustain 0.8
                      release 1
                      gate 1]
    (let [env (env-gen (adsr attack decay sustain release) gate :action FREE)
          mod (lin-lin:kr
               :in (sin-osc:kr 6)
               :scrlo -1            :srchi 1
               :dstlo (* freq 0.99) :dsthi (* freq 1.01))
          ] (* env (sin-osc [freq mod]))))

  (defcgen midihzgen [in {:default nil :doc "The input signal"}]
    (:ar (midi->hz in)))

  (distorter :sustain 0.3)

  (do
    (stop)
    (definst test [freq 440]
      (sin-osc   (/ freq 1)))
    (test)
    )

  (stop)
  (volume 100)
  (foo)
  ()

  (lp-bind-octave foo 7)
  (lp-bind-octave distorter 7)

  (ctl modulation-test :depth 50)

  (stop)

  (pulse))
