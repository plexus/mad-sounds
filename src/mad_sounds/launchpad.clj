(ns mad-sounds.launchpad
  (:require [overtone.live :refer :all])
  (:require [overtone.inst.drum :refer :all])
  (:require [launchpad.core :refer :all])
  (:require [overtone.studio.scope :as scope])
  (:require [overtone.inst.sampled-piano :refer :all]))

(definst ding
  [note 60 velocity 100 gate 1]
  (let [freq (midicps note)
        amp  (/ velocity 127.0)
        snd  (sin-osc freq)
        env  (env-gen (adsr 0.001 0.1 0.6 0.3) gate :action FREE)]
    (* amp env snd)))
(ding)
(stop)

    (def dinger (midi-poly-player ding))


(def player (midi-poly-player sampled-piano))

(kick)
(snare)

;; C-c M-j cider-jack-in
;; C-c M-n cider-repl-set-ns
;; C-c C-r cider-eval-region
;; C-M-x   cider "eval outer sexp"

;; (use '[clojure.tools.namespace.repl :only (refresh)])

(do
  (boot!)
  (require '[launchpad.device :as device])
  (require '[launchpad.state-maps :as state])
  (require '[launchpad.plugin.metronome :as metronome])
  (use '[overtone.helpers.lib :only [uuid]])

  (defonce root-trg-bus (control-bus)) ;; global metronome pulse
  (defonce root-cnt-bus (control-bus)) ;; global metronome count
  (defonce beat-trg-bus (control-bus)) ;; beat pulse (fraction of root)
  (defonce beat-cnt-bus (control-bus)) ;; beat count

  (defonce count-trig-id (trig-id))
  (defonce beat-rep-key (uuid))

  (def BEAT-FRACTION "Number of global pulses per beat" 30)

  (defsynth root-trg [rate 100]
    (out:kr root-trg-bus (impulse:kr rate)))

  (defsynth root-cnt []
    (out:kr root-cnt-bus (pulse-count:kr (in:kr root-trg-bus))))
  (defsynth beat-trg [div BEAT-FRACTION]
    (out:kr beat-trg-bus (pulse-divider (in:kr root-trg-bus) div))  )
  (defsynth beat-cnt []
    (out:kr beat-cnt-bus (pulse-count (in:kr beat-trg-bus))))

  (def r-trg (root-trg))
  (def r-cnt (root-cnt [:after r-trg]))
  (def b-trg (beat-trg [:after r-trg]))
  (def b-cnt (beat-cnt [:after b-trg]))

  ;; Metronome!
  (def lp (first launchpad-kons))

  (defsynth get-beat []
    (send-trig (in:kr beat-trg-bus) count-trig-id (+ (in:kr beat-cnt-bus) 1)))

  (defonce get-beat-s (get-beat))

  ;;(metronome/start lp :mixer count-trig-id beat-rep-key)

  (def top-row-buttons [:1 :2 :3 :4 :5 :6 :7 :8])

  (on-trigger count-trig-id (fn [beat]
                              (doseq [button top-row-buttons] (device/led-off lp button))
                              (let [note (int (mod beat 8))]
                                (device/led-on lp (top-row-buttons note) 3 :green)
                                ))
              beat-rep-key))

(state/on? (:state lp) 0 0)

(comment
  (defonce buf-0 (buffer 8))
  (defonce buf-1 (buffer 8))
  (defonce buf-2 (buffer 8))
  (defonce buf-3 (buffer 8))

  (defonce root-trg-bus (control-bus)) ;; global metronome pulse
  (defonce root-cnt-bus (control-bus)) ;; global metronome count
  (defonce beat-trg-bus (control-bus)) ;; beat pulse (fraction of root)
  (defonce beat-cnt-bus (control-bus)) ;; beat count

  (require '[overtone.synth.timing :as timing])

  (defonce r-trg (timing/trigger :rate 100 :in-bus root-trg-bus))

  (def BEAT-FRACTION "Number of global pulses per beat" 30)

  (defsynth root-trg [rate 100] (out:kr root-trg-bus (impulse:kr rate)))
  (defsynth root-cnt [] (out:kr root-cnt-bus (pulse-count:kr (in:kr root-trg-bus))))
  (defsynth beat-trg [div BEAT-FRACTION] (out:kr beat-trg-bus (pulse-divider (in:kr root-trg-bus) div))  )
  (defsynth beat-cnt [] (out:kr beat-cnt-bus (pulse-count (in:kr beat-trg-bus))))

  (def kick-s (freesound 777))
  (def click-s (freesound 406))
  (def boom-s (freesound 33637))
  (def subby-s (freesound 25649))

  ;; Here's a synth for playing back the samples with a bit of modulation
  ;; to keep things interesting.
  (defsynth mono-sequencer
    "Plays a single channel audio buffer."
    [buf 0 rate 1 out-bus 0 beat-num 0 sequencer 0 amp 1]
    (let [cnt      (in:kr beat-cnt-bus)
          beat-trg (in:kr beat-trg-bus)
          bar-trg  (and (buf-rd:kr 1 sequencer cnt)
                        (= beat-num (mod cnt 8))
                        beat-trg)
          vol      (set-reset-ff bar-trg)]
      (out
       out-bus (* vol
                  amp
                  (pan2
                   (rlpf
                    (scaled-play-buf 1 buf rate bar-trg)
                    (demand bar-trg 0 (dbrown 200 20000 50 INF))
                    (lin-lin:kr (lf-tri:kr 0.01) -1 1 0.1 0.9)))))))

  ;; Here's Dan Stowell's dubstep synth modified to work with the global
  ;; pulses
  (definst dubstep [note 40 wobble BEAT-FRACTION hi-man 0 lo-man 0 sweep-man 0 deci-man 0 tan-man 0 shape 0 sweep-max-freq 3000 hi-man-max 1000 lo-man-max 500 beat-vol 0 lag-delay 0.5]
    (let [bpm     300
          wob     (pulse-divider (in:kr root-trg-bus) wobble)
          sweep   (lin-lin:kr (lag-ud wob 0.01 lag-delay) 0 1 400 sweep-max-freq)
          snd     (mix (saw (* (midicps note) [0.99 1.01])))
          snd     (lpf snd sweep)
          snd     (normalizer snd)

          snd     (bpf snd 1500 2)
          ;;special flavours
          ;;hi manster
          snd     (select (> hi-man 0.05) [snd (* 4 (hpf snd hi-man-max))])

          ;;sweep manster
          snd     (select (> sweep-man 0.05) [snd (* 4 (hpf snd sweep))])

          ;;lo manster
          snd     (select (> lo-man 0.05) [snd (lpf snd lo-man-max)])

          ;;decimate
          snd     (select (> deci-man 0.05) [snd (round snd 0.1)])

          ;;crunch
          snd     (select (> tan-man 0.05) [snd (tanh (* snd 5))])

          snd     (* 0.5 (+ (* 0.8 snd) (* 0.3 (g-verb snd 100 0.7 0.7))))
          ]
      (normalizer snd)))

  ;; Here's a nice supersaw synth
  (definst supersaw2 [freq 440 amp 1 fil-mul 2 rq 0.3]
    (let [input  (lf-saw freq)
          shift1 (lf-saw 4)
          shift2 (lf-saw 7)
          shift3 (lf-saw 5)
          shift4 (lf-saw 2)
          comp1  (> input shift1)
          comp2  (> input shift2)
          comp3  (> input shift3)
          comp4  (> input shift4)
          output (+ (- input comp1)
                    (- input comp2)
                    (- input comp3)
                    (- input comp4))
          output (- output input)
          output (leak-dc:ar (* output 0.25))
          output (normalizer (rlpf output (* freq fil-mul) rq))]

      (* amp output (line 1 0 10 FREE))))


  ;; OK, let's make some noise!

  ;; Now, let's start up all the synths:
  (do
    (def r-trg (root-trg))
    (def r-cnt (root-cnt [:after r-trg]))
    (def b-trg (beat-trg [:after r-trg]))
    (def b-cnt (beat-cnt [:after b-trg]))


    (def kicks (doall
                (for [x (range 8)]
                  (mono-sequencer :buf kick-s :beat-num x :sequencer buf-0))))

    (def clicks (doall
                 (for [x (range 8)]
                   (mono-sequencer :buf click-s :beat-num x :sequencer buf-1))))

    (def booms (doall
                (for [x (range 8)]
                  (mono-sequencer :buf boom-s :beat-num x :sequencer buf-2))))

    (def subbies (doall
                  (for [x (range 8)]
                    (mono-sequencer :buf subby-s :beat-num x :sequencer buf-3)))))

  ;; An empty palatte to play with:
  (do
    (buffer-write! buf-0 [1 0 0 0 0 1 0 0])  ;; kickq
    (buffer-write! buf-1 [0 0 1 0 0 0 1 0])  ;; click
    (buffer-write! buf-2 [0 0 0 0 1 0 0 0])  ;; boom
    (buffer-write! buf-3 [0 0 0 0 0 0 0 0])) ;; subby

  ;; try mixing up the sequences. Evaluate this a few times:
  (do
    (buffer-write! buf-0 (repeatedly 8 #(choose [0 1])))
    (buffer-write! buf-1 (repeatedly 8 #(choose [0 1])))
    (buffer-write! buf-2 (repeatedly 8 #(choose [0 1])))
    (buffer-write! buf-3 (repeatedly 8 #(choose [0 1]))))

  ;; and then to something interesting
  (do
    (buffer-write! buf-0 [1 1 1 1 1 1 1 1])
    (buffer-write! buf-1 [1 0 1 0 0 1 1 0])
    (buffer-write! buf-2 [1 1 0 1 0 1 1 0])
    (buffer-write! buf-3 [1 0 0 0 0 0 1 0]))

  (do
    (buffer-write! buf-0 [0 0 0 0 0 0 0 0])
    (buffer-write! buf-1 [0 0 0 0 0 0 0 0])
    (buffer-write! buf-2 [0 0 0 0 0 0 0 0])
    (buffer-write! buf-3 [0 0 0 0 0 0 0 0]))

  (stop)

  ;; try changing the rate of the global pulse (everything else will
  ;; follow suit):
  (ctl r-trg :rate 75)
  (ctl r-trg :rate 300)
  (ctl r-trg :rate 150)

  ;; get the dubstep bass involved:
  (dubstep :note 28
           :wobble (* BEAT-FRACTION 1)
           :lo-man 1)

  ;; go crazy - especially with the deci-man
  (ctl dubstep
       :note 40
       :wobble (* BEAT-FRACTION 0.1)
       :lag-delay 0.05
       :hi-man 0
       :lo-man 0
       :deci-man 0)


  ;; Bring in the supersaws!

  (def ssaw-rq 0.9)
  (def ssaw-fil-mul 2)

  ;; Fire at will...
  (supersaw2 (midi->hz 28) :amp 3 :fil-mul ssaw-fil-mul :rq ssaw-rq)
  (supersaw2 (midi->hz 40) :amp 3 :fil-mul ssaw-fil-mul :rq ssaw-rq)
  (supersaw2 (midi->hz 45) :amp 2 :fil-mul ssaw-fil-mul :rq ssaw-rq)
  (supersaw2 (midi->hz 48) :amp 2 :fil-mul ssaw-fil-mul :rq ssaw-rq)
  (supersaw2 (midi->hz 52) :amp 2 :fil-mul ssaw-fil-mul :rq ssaw-rq)
  (supersaw2 (midi->hz 55) :amp 2 :fil-mul ssaw-fil-mul :rq ssaw-rq)
  (supersaw2 (midi->hz 57) :amp 2 :fil-mul ssaw-fil-mul :rq ssaw-rq)
  (supersaw2 (midi->hz 64) :amp 1 :fil-mul ssaw-fil-mul :rq ssaw-rq)
  (supersaw2 (midi->hz 67) :amp 1 :fil-mul ssaw-fil-mul :rq ssaw-rq)
  (supersaw2 (midi->hz 69) :amp 1 :fil-mul ssaw-fil-mul :rq ssaw-rq))

;; modify saw params on the fly too...
;;(ctl supersaw2 :fil-mul 4 :rq 0.2)
;;(stop)



(comment
  (definst plexus-kick [freq 120 dur 0.5 width 0.5]
    (let [freq-env (* freq (env-gen (perc 0 (* 0.99 dur))))
          env (env-gen (perc 0.01 dur) 1 1 0 1 FREE)
          sqr (* (env-gen (perc 0 0.01)) (pulse (* 2 freq) width))
          src (sin-osc freq-env)
          drum (+ sqr (* env src))]
      (compander drum drum 0.2 1 0.1 0.01 0.01)))

  ;;Bind to the main grid
  (bind :up :0x0 #(kick))
  (bind :up :0x1 #(hat3))
  (bind :up :0x2 #(plexus-kick))

  (definst beep [note 60 vol 1]
    (let
        [src (sin-osc (midicps note))
         env (env-gen (perc 0.01 0.3) :action FREE)]
      (* src env)))

  (beep)

  (definst weirdos []
    (let [noise (lf-noise1 3)
          saws  (mul-add (lf-saw [5 5.123]) 3 80)
          freq  (midicps (mul-add noise 24 saws))
          src   (* 0.4 (sin-osc freq))]
      (comb-n src 1 0.3 2)))

(weirdos)
(stop)
  (bind :up :1x2 #(weirdos))
  (bind :up :1x1 #(stop))


  (require '[launchpad.state-maps :as state-maps])

  (state-maps/toggle!)
  (state-maps/toggle! (:state (first launchpad.core/launchpad-kons)) 2 2)

  (ctl (foundation-output-group) :master-volume 1)

)
