(ns mad-sounds.scratch
  (:require [overtone.live :refer :all]
            [overtone.inst.synth :refer :all]
            [mad-sounds.inst.sampled :refer :all]
            [mad-sounds.inst.synths :refer :all]))


(definst emacsberlinsynth [note 60]
  (* (env-gen (perc))
     (saw (midicps note))))

(emacsberlinsynth (note :f3))
(stop)



(definst oscilophone [note 60 duration 1.0 level 1.0
                      attack 0.1 decay 0.2 sustain 0.5 release 0.05
                      gate 1]
  (* level
     (env-gen (adsr attack decay sustain release) :gate gate)
     (sin-osc (midicps note))))

(def synth1 (synth-maker :note (note :c5) ))
(def synth2 (synth-maker :note (note :e5) ))
(def synth3 (synth-maker :note (note :g5) ))

(ctl playing-synth :gate 1)
(ctl playing-synth :note (note :g5))


(let [name1 75
      name2 99]
  (synth-maker :note name1)
  (synth-maker :note name2))

(synth-maker :note name1)

(enveloped-synth :note )
(stop)






























(def kick-fat (freesound 2086))
(def snare-fat (freesound 122053))

;; bpm -> (beat-nr -> time-ms)
(start-beat (metronome 120))

(defn ld [beat]
  (+ beat 3))

(def ld (fn [x y]
          (+ x y)))

(ld 3 5)

((fn [x y] (+ x y)) 3 5)

(defn from [m beat]
  (fn [& beat]
    (if beat
      (m (+ 2 beat))
      (+ (m) 2))))

(let [m     (metronome 120)
      new-m (from m 2)]
  m ;; (beat-nr -> time-ms)|( -> next-beat-nr)
  (m)
  (m 2)

  new-m ;; (beat-nr -> time-ms)
  (new-m)
  (new-m 2)
  )

;; bpm -> (beat-nr -> time-ms)|( -> next-beat-nr)
;; bpm -> (beat-nr -> time-ms)|( -> next-beat-nr)

(defn start-beat [m]
  (at (m (+ (m) 0)) (kick-fat))
  (at (m (+ (m) 1)) (snare-fat))
  (apply-by (m (+ (m) 2)) start-beat [m]))

(let [ruby-monster {:name "Bettina" :mad-skillz ["beats"]}]
  (:name ruby-monster))

(:foo {:foo 7})
(get {:foo 7} :foo)

(get-in
 {:foo 7
  :bar [1 2 {:key :value}]}

 [:bar 2 :key])

(defn start-beat [m]
  (at (m 0) ((rand-nth [kick-fat snare-fat])))
  (at (m 1) (snare-fat))
  (apply-by (m 2) start-beat [(from m 2)]))

(start-beat (metronome 120))




;; => [1.408385469554E12 1.408385470054E12 1.408385470554E12]




(stop)

(kick-fat)
(snare-fat)
































(pw-gong)


(env-gen (envelope [0,1,1,0] [0.75*dr, 0.24*dr, 0.01*dr]))

(comment
  ;; Scheme/snd version of a gong
  (definstrument (gong start-time duration frequency amplitude
                       (degree 0.0) (distance 1.0) (reverb-amount 0.005))
    (let ((mfq1 (* frequency 1.16))
          (mfq2 (* frequency 3.14))
          (mfq3 (* frequency 1.005)))
      (let ((indx01 (hz->radians (* .01 mfq1)))
            (indx11 (hz->radians (* .30 mfq1)))
            (indx02 (hz->radians (* .01 mfq2)))
            (indx12 (hz->radians (* .38 mfq2)))
            (indx03 (hz->radians (* .01 mfq3)))
            (indx13 (hz->radians (* .50 mfq3)))
            (atpt 5)
            (atdur (* 100 (/ .002 duration)))
            (expf '(0 0  3 1  15 .5  27 .25  50 .1  100 0))
            (rise '(0 0  15 .3  30 1.0  75 .5  100 0))
            (fmup '(0 0  75 1.0  98 1.0  100 0))
            (fmdwn '(0 0  2 1.0  100 0)))
        (let ((ampfun (make-env (stretch-envelope expf atpt atdur)
                                :scaler amplitude :duration duration))
              (indxfun1 (make-env fmup :duration duration
                                  :scaler (- indx11 indx01) :offset indx01))
              (indxfun2 (make-env fmdwn :duration duration
                                  :scaler (- indx12 indx02) :offset indx02))
              (indxfun3 (make-env rise :duration duration
                                  :scaler (- indx13 indx03) :offset indx03))
              (loc (make-locsig degree distance reverb-amount))
              (carrier (make-oscil frequency))
              (mod1 (make-oscil mfq1))
              (mod2 (make-oscil mfq2))
              (mod3 (make-oscil mfq3))
              (beg (seconds->samples start-time))
              (end (seconds->samples (+ start-time duration))))
          (do ((i beg (+ i 1)))
              ((= i end))
              (locsig loc i (* (env ampfun)
                               (oscil carrier (+ (* (env indxfun1) (oscil mod1))
                                                 (* (env indxfun2) (oscil mod2))
                                                 (* (env indxfun3) (oscil mod3))))))))))))

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
