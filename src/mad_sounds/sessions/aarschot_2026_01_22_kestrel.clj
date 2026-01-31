(ns mad-sounds.sessions.aarschot-2026-01-22-kestrel
  (:require
   [casa.squid.jack :as jack]
   [clojure.java.io :as io]
   [overtone.live :refer :all]
   [vibeflow.util :as util :refer [ctl!]]))

(jack/connect!
 #{["Overtone:out_1" "Built-in Audio Analog Stereo:playback_FL"]
   ["Overtone:out_2" "Built-in Audio Analog Stereo:playback_FR"]
   })

;; hi-hat
(demo (-> (white-noise)
          (+ (* 0.3 (square (lin-lin (env-gen (adsr 0 0.06 0 0))
                                     1 0
                                     8000 1000))))
          (* (env-gen (adsr 0.01 0.06 0 0)))
          (hpf 1500)
          (rlpf (lin-lin (env-gen (adsr 0 0.04 0 0))
                         1 0
                         13000 5000)
                0.4)))

(demo (-> (square 200)
          (* (env-gen (adsr 0.01 0.1 0 0)))
          #_(hpf 1500)
          (rlpf (lin-lin (env-gen (adsr 0 0.1 0 0))
                         0 1 ;; reversed
                         7000 0)
                1)))

(midi->hz 48)

(definst ym2612-0
  "Mode 0: single stack, 1 -> 2 -> 3 -> 4"
  [freq 130.80
   amp 1.0
   gate 1.0
   ;; feedback
   fb 0.0
   ;; level
   l1 1.0
   l2 1.0
   l3 1.0
   l4 1.0
   ;; multiplier
   m1 1.0
   m2 1.0
   m3 1.0
   m4 1.0
   ;; env
   a1 0.0
   a2 0.0
   a3 0.0
   a4 0.0
   d1 0.2
   d2 0.3
   d3 0.4
   d4 0.5
   s1 0.3
   s2 0.3
   s3 0.3
   s4 0.7
   r1 0.1
   r2 0.1
   r3 0.1
   r4 0.1
   ;; detune
   dt1 0.0
   dt2 0.0
   dt3 0.0
   dt4 0.0
   ;; phase scaling
   pm-scale 4.0]
  (let [env1 (adsr a1 d1 s1 r1)
        env2 (adsr a2 d2 s2 r2)
        env3 (adsr a3 d3 s3 r3)
        env4 (adsr a4 d4 s4 r4)
        op1 (* l1 pm-scale (fb-sine-n (* (+ m1 dt1) freq) fb) (env-gen env1))
        op2 (* l2 pm-scale (sin-osc (* (+ m2 dt2) freq) op1) (env-gen env2))
        op3 (* l3 pm-scale (sin-osc (* (+ m3 dt3) freq) op2) (env-gen env3))
        op4 (* amp l4 (sin-osc (* m4 freq) op3)  (env-gen env4 gate :action FREE))]
    op4))




(ym2612-series (midi->hz 42) :m1 12 :m2 8)
(ym2612-series (midi->hz 42) :m1 4.0 :m2 2.0 :d1 0.1 :d2 0.2 :dt3 0.05)
(ym2612-series (midi->hz 34) :fb 2.5 :l1 2.0 :m1 1.0 :m2 1.0 :l2 1.5 :d1 0.2)
(stop)

(ym2612-series (midi->hz 40) :fb 20.0 :l1 4.0 :d1 0.05 :s1 0.0 :l4 1.0 :d4 0.1)
(stop)
(doseq [i [30 32 34 35 37 39 41 42]]
  (let [y (ym2612-series (midi->hz i)
                         :fb 2.5
                         :m1 1.0 :l1 4.0 :d1 0.1 :s1 0.0  ;; "The Growl"
                         :m2 1.0 :l2 2.0 :d2 0.2 :s2 0.0  ;; "The Body"
                         :m3 2.0 :l3 1.5 :d3 0.1 :s3 0.0  ;; "The Pluck"
                         :m4 1.0 :l4 1.0 :d4 0.5 :s4 0.5)]
    (Thread/sleep 200)
    (ctl y :gate 0)))

(doseq [i [54 56 58 59 61 63 65 66]]
  (let [y (ym2612-series (midi->hz i)
                         :m1 1.0 :l1 0.8 :a1 0.05 :s1 0.6
                         :m2 2.0 :l2 1.2 :a2 0.08 :s2 0.5
                         :m3 1.0 :l3 0.5 :a3 0.10 :s3 0.4
                         :m4 1.0 :l4 1.0 :a4 0.02 :s4 0.8)]
    (Thread/sleep 200)
    (ctl y :gate 0)))

(doseq [i [54 56 58 59 61 63 65 66]]
  (let [y
        (ym2612-series (midi->hz i)
                       :m1 8.0 :l1 2.0 :d1 0.05 :s1 0.0  ;; High chime
                       :m2 1.0 :l2 1.5 :d2 0.15 :s2 0.0  ;; Mid warmth
                       :m3 1.0 :l3 0.5 :d3 0.40 :s3 0.0  ;; Fundamental
                       :m4 1.0 :l4 1.0 :d4 0.80 :s4 0.2)]
    (Thread/sleep 200)
    (ctl y :gate 0)))


(doseq [i [54 56 58 59 61 63 65 66]]
  (let [y (ym2612-series (midi->hz i)
                         :m1 1.0 :l1 0.5 :dt1 0.02
                         :m2 1.0 :l2 0.8 :dt2 0.01
                         :m3 1.0 :l3 5.0 :d3 0.1 :s3 0.7   ;; High index = bright buzz
                         :m4 1.0 :l4 1.0 :a4 0.01 :s4 0.9)]
    (Thread/sleep 200)
    (ctl y :gate 0)))

(doseq [i [54 56 58 59 61 63 65 66]]
  (let [y (ym2612-series (midi->hz i)
                         :m1 1.0 :l1 0.2 :dt1 0.05 :a1 0.2
                         :m2 3.0 :l2 0.1 :dt2 0.08 :a2 0.1  ;; Breath noise
                         :m3 1.0 :l3 0.1 :a3 0.2
                         :m4 1.0 :l4 1.0 :a4 0.1 :s4 1.0)]
    (Thread/sleep 200)
    (ctl y :gate 0)))

;; Body

(do
  (ym2612-series (midi->hz 42) :m1 12 :m2 8)
  (Thread/sleep 300)
  (ctl ym2612-series :gate 0))

(ym2612-series (midi->hz 42) :amp 1.5)
(ctl ym2612-series :gate 0)
(stop)

(fb-sine-c)
(fb-sine-l)
(fb-sine-n)

(dbamp (* -0.75 l1))
(db->amp (* -0.75 10))

(definst ym2612-mode0
  "Approximation of the Yamaha YM2612 used in the Sega Genesis. The YM2612 has 4
  'Operators' (sine oscillators), which can either acts as 'carrier' (output
  signal) or modulator, phase modulating another operator. This synth models
  mode 0, where all four oscillators are 'stacked', each modulating the next
  one. Parameters match the range the YM2612 used. e.g. 0-127, 0-32 etc. This
  makes it easy to map these to MIDI CC values.

  Note that the fourth operator is the carrier, which is modulated by op3, etc.
  So you want to tweak `:l3`/`:m3` before moving on to `:l2` and `:l1`.

  The default has operators 1/2/3 (the modulators) at minimum volume, so you get
  a clean sine wave. Decrease the value of :l3/:l2/:l1 (in that order) and
  you'll get richer timbres. Similarly set :m3/:m2/:m1 to change the frequency
  multiplier for each operator, and get other timbres as well.

  Operator 1 (the start of the chain) can be set to feed back into itself,
  leading to noise/crunch."
  [;; conventional synth params
   freq 130.80
   amp 1.0
   gate 1.0
   ;; feedback
   fb 0.0

   ;; Operators
   ;; --- Levels (Total Level) ---
   ;; 0 is max volume (0dB), 127 is silence (-95.25dB)
   l1 {:doc "op1 level (modulator)" :min 0 :max 127 :default 127}
   l2 {:doc "op2 level (modulator)" :min 0 :max 127 :default 127}
   l3 {:doc "op3 level (modulator)" :min 0 :max 127 :default 127}
   l4 {:doc "op4 level (carrier)" :min 0 :max 127 :default 0}

   ;; --- Multipliers ---
   ;; 0 maps to 0.5, 1-15 map to integers 1-15
   m1 {:doc "op1 freq multiplier" :min 0 :max 15 :default 1}
   m2 {:doc "op2 freq multiplier" :min 0 :max 15 :default 1}
   m3 {:doc "op3 freq multiplier" :min 0 :max 15 :default 1}
   m4 {:doc "op4 freq multiplier" :min 0 :max 15 :default 1}

   ;; --- Attack Rates ---
   ;; 31 is instant, 0 is approx 10+ seconds
   a1 {:doc "op1 attack rate" :min 0 :max 31 :default 31}
   a2 {:doc "op2 attack rate" :min 0 :max 31 :default 31}
   a3 {:doc "op3 attack rate" :min 0 :max 31 :default 31}
   a4 {:doc "op4 attack rate" :min 0 :max 31 :default 31}

   ;; --- Decay Rates ---
   ;; 31 is fastest decay, 0 is approx 10+ seconds
   d1 {:doc "op1 decay rate" :min 0 :max 31 :default 31}
   d2 {:doc "op2 decay rate" :min 0 :max 31 :default 31}
   d3 {:doc "op3 decay rate" :min 0 :max 31 :default 31}
   d4 {:doc "op4 decay rate" :min 0 :max 31 :default 31}

   ;; --- Sustain Levels ---
   ;; 0 is max volume (peak), 127 is ~silence
   s1 {:doc "op1 sustain level" :min 0 :max 127 :default 0}
   s2 {:doc "op2 sustain level" :min 0 :max 127 :default 0}
   s3 {:doc "op3 sustain level" :min 0 :max 127 :default 0}
   s4 {:doc "op4 sustain level" :min 0 :max 127 :default 0}

   ;; --- Release Rates ---
   ;; 31 is fastest release, 0 is approx 10+ seconds
   r1 {:doc "op1 release rate" :min 0 :max 31 :default 31}
   r2 {:doc "op2 release rate" :min 0 :max 31 :default 31}
   r3 {:doc "op3 release rate" :min 0 :max 31 :default 31}
   r4 {:doc "op4 release rate" :min 0 :max 31 :default 31}

   ;; detune
   dt1 0.0 dt2 0.0 dt3 0.0 dt4 0.0

   ;; phase scaling constant, you shouldn't have to mess with this normally. It
   ;; influences how sensitive m1/m2/m3/m4 are
   pm-scale 4.0]
  (let [env1 (adsr (lin-exp a1 0 31 10.0 0.001)
                   (lin-exp d1 0 31 10.0 0.001)
                   (lin-lin s1 0 127 1.0 0.0)
                   (lin-exp r1 0 31 10.0 0.001))
        env2 (adsr (lin-exp a2 0 31 10.0 0.001)
                   (lin-exp d2 0 31 10.0 0.001)
                   (lin-lin s2 0 127 1.0 0.0)
                   (lin-exp r2 0 31 10.0 0.001))
        env3 (adsr (lin-exp a3 0 31 10.0 0.001)
                   (lin-exp d3 0 31 10.0 0.001)
                   (lin-lin s3 0 127 1.0 0.0)
                   (lin-exp r3 0 31 10.0 0.001))
        env4 (adsr (lin-exp a4 0 31 10.0 0.001)
                   (lin-exp d4 0 31 10.0 0.001)
                   (lin-lin s4 0 127 1.0 0.0)
                   (lin-exp r4 0 31 10.0 0.001))
        m1'  (+ m1 (* 0.5 (< m1 1))) ;; 0=0.5, higher -> 1=1
        m2'  (+ m2 (* 0.5 (< m2 1)))
        m3'  (+ m3 (* 0.5 (< m3 1)))
        m4'  (+ m4 (* 0.5 (< m4 1)))
        op1  (* (dbamp (* -0.75 l1)) pm-scale
                (fb-sine-n (* (+ m1' dt1) freq) (* fb (/ Math/PI 7.0)))
                (env-gen env1 gate))
        op2  (* (dbamp (* -0.75 l2)) pm-scale (sin-osc (* (+ m2' dt2) freq) op1) (env-gen env2 gate))
        op3  (* (dbamp (* -0.75 l3)) pm-scale (sin-osc (* (+ m3' dt3) freq) op2) (env-gen env3 gate))
        op4  (* amp (dbamp (* -0.75 l4)) (sin-osc (* m4' freq) op3) (env-gen env4 gate :action FREE))]
    op4))


(definst ym2612-mode4
  "Approximation of the Yamaha YM2612 used in the Sega Genesis. The YM2612 has 4
  'Operators' (sine oscillators), which can either acts as 'carrier' (output
  signal) or modulator, phase modulating another operator. This synth models
  mode 0, where all four oscillators are 'stacked', each modulating the next
  one. Parameters match the range the YM2612 used. e.g. 0-127, 0-32 etc. This
  makes it easy to map these to MIDI CC values.

  Note that the fourth operator is the carrier, which is modulated by op3, etc.
  So you want to tweak `:l3`/`:m3` before moving on to `:l2` and `:l1`.

  The default has operators 1/2/3 (the modulators) at minimum volume, so you get
  a clean sine wave. Decrease the value of :l3/:l2/:l1 (in that order) and
  you'll get richer timbres. Similarly set :m3/:m2/:m1 to change the frequency
  multiplier for each operator, and get other timbres as well.

  Operator 1 (the start of the chain) can be set to feed back into itself,
  leading to noise/crunch."
  [;; conventional synth params
   freq 130.80
   amp 1.0
   gate 1.0
   ;; feedback
   fb 0.0

   ;; Operators
   ;; --- Levels (Total Level) ---
   ;; 0 is max volume (0dB), 127 is silence (-95.25dB)
   l1 {:doc "op1 level (modulator)" :min 0 :max 127 :default 127}
   l2 {:doc "op2 level (modulator)" :min 0 :max 127 :default 127}
   l3 {:doc "op3 level (modulator)" :min 0 :max 127 :default 127}
   l4 {:doc "op4 level (carrier)" :min 0 :max 127 :default 0}

   ;; --- Multipliers ---
   ;; 0 maps to 0.5, 1-15 map to integers 1-15
   m1 {:doc "op1 freq multiplier" :min 0 :max 15 :default 1}
   m2 {:doc "op2 freq multiplier" :min 0 :max 15 :default 1}
   m3 {:doc "op3 freq multiplier" :min 0 :max 15 :default 1}
   m4 {:doc "op4 freq multiplier" :min 0 :max 15 :default 1}

   ;; --- Attack Rates ---
   ;; 31 is instant, 0 is approx 10+ seconds
   a1 {:doc "op1 attack rate" :min 0 :max 31 :default 31}
   a2 {:doc "op2 attack rate" :min 0 :max 31 :default 31}
   a3 {:doc "op3 attack rate" :min 0 :max 31 :default 31}
   a4 {:doc "op4 attack rate" :min 0 :max 31 :default 31}

   ;; --- Decay Rates ---
   ;; 31 is fastest decay, 0 is approx 10+ seconds
   d1 {:doc "op1 decay rate" :min 0 :max 31 :default 31}
   d2 {:doc "op2 decay rate" :min 0 :max 31 :default 31}
   d3 {:doc "op3 decay rate" :min 0 :max 31 :default 31}
   d4 {:doc "op4 decay rate" :min 0 :max 31 :default 31}

   ;; --- Sustain Levels ---
   ;; 0 is max volume (peak), 127 is ~silence
   s1 {:doc "op1 sustain level" :min 0 :max 127 :default 0}
   s2 {:doc "op2 sustain level" :min 0 :max 127 :default 0}
   s3 {:doc "op3 sustain level" :min 0 :max 127 :default 0}
   s4 {:doc "op4 sustain level" :min 0 :max 127 :default 0}

   ;; --- Release Rates ---
   ;; 31 is fastest release, 0 is approx 10+ seconds
   r1 {:doc "op1 release rate" :min 0 :max 31 :default 31}
   r2 {:doc "op2 release rate" :min 0 :max 31 :default 31}
   r3 {:doc "op3 release rate" :min 0 :max 31 :default 31}
   r4 {:doc "op4 release rate" :min 0 :max 31 :default 31}

   ;; detune
   dt1 0.0 dt2 0.0 dt3 0.0 dt4 0.0

   ;; phase scaling constant, you shouldn't have to mess with this normally. It
   ;; influences how sensitive m1/m2/m3/m4 are
   pm-scale 4.0]
  (let [env1 (adsr (lin-exp a1 0 31 10.0 0.001)
                   (lin-exp d1 0 31 10.0 0.001)
                   (lin-lin s1 0 127 1.0 0.0)
                   (lin-exp r1 0 31 10.0 0.001))
        env2 (adsr (lin-exp a2 0 31 10.0 0.001)
                   (lin-exp d2 0 31 10.0 0.001)
                   (lin-lin s2 0 127 1.0 0.0)
                   (lin-exp r2 0 31 10.0 0.001))
        env3 (adsr (lin-exp a3 0 31 10.0 0.001)
                   (lin-exp d3 0 31 10.0 0.001)
                   (lin-lin s3 0 127 1.0 0.0)
                   (lin-exp r3 0 31 10.0 0.001))
        env4 (adsr (lin-exp a4 0 31 10.0 0.001)
                   (lin-exp d4 0 31 10.0 0.001)
                   (lin-lin s4 0 127 1.0 0.0)
                   (lin-exp r4 0 31 10.0 0.001))
        m1'  (+ m1 (* 0.5 (< m1 1))) ;; 0=0.5, higher -> 1=1
        m2'  (+ m2 (* 0.5 (< m2 1)))
        m3'  (+ m3 (* 0.5 (< m3 1)))
        m4'  (+ m4 (* 0.5 (< m4 1)))
        op1  (* (dbamp (* -0.75 l1)) pm-scale
                (fb-sine-n (* (+ m1' dt1) freq) (* fb (/ Math/PI 7.0)))
                (env-gen env1 gate))
        op2  (* (dbamp (* -0.75 l2)) pm-scale (sin-osc (* (+ m2' dt2) freq) op1) (env-gen env2 gate))
        op3  (* (dbamp (* -0.75 l3)) pm-scale (sin-osc (* (+ m3' dt3) freq)) (env-gen env3 gate))
        op4  (* amp (dbamp (* -0.75 l4)) (sin-osc (* m4' freq) op3) (env-gen env4 gate :action FREE))]
    (mix [op2 op4])))

(format "%f"
        (db->amp (* -0.75 200)))

(ym2612-m :freq 440 :l3 64 :l2 0 :l1 0
          )
(stop)


(ym2612-m (midi->hz 60) :fb 6 :pm-scale 6.0
          :m1 1 :l1 30 :a1 31 :d1 20 :s1 20
          :m2 1 :l2 35 :a2 31 :d2 25 :s2 30
          :m3 1 :l3 40 :a3 31 :d3 25 :s3 40
          :m4 1 :l4 0  :a4 31 :d4 31 :s4 10
          )
(stop)
(event {:type :note
        :instrument ym2612-m
        :midinote 60
        :m1 1 :l1 30 :a1 31 :d1 20 :s1 20
        :m2 1 :l2 35 :a2 31 :d2 25 :s2 30
        :m3 1 :l3 40 :a3 31 :d3 25 :s3 40
        :m4 1 :l4 0  :a4 31 :d4 31 :s4 10}
       )

(clojure.reflect/reflect ym2612-m)

(stop)
(ploop :x
       {:instrument ym2612-m
        :octave 4
        :degree [1 3 4]
        :dur [1 1/2 1/2]

        ;; :m1 1 :l1 35 :a1 20 :d1 25 :s1 20  ;; "Bloom" modulators
        ;; :m2 2 :l2 45 :a2 18 :d2 25 :s2 30
        ;; :m3 1 :l3 50 :a3 22 :d3 25 :s3 40
        ;; :m4 1 :l4 0  :a4 31 :d4 31 :s4 10

        ;; :pm-scale 8.0
        ;; :m1 12 :l1 20 :dt1 0.1 :a1 31 :d1 25 :s1 127
        ;; :m2 7  :l2 30 :dt2 0.05 :a2 31 :d2 20 :s2 127
        ;; :m3 3  :l3 40 :a3 31 :d3 18 :s3 127
        ;; :m4 1  :l4 0  :a4 31 :d4 12 :s4 127 :r4 15

        ;; :pm-scale 5.0
        ;; :m1 0 :l1 25 :a1 31 :d1 22 :s1 127 ;; Quick 0.5 ratio pluck
        ;; :m2 1 :l2 35 :a2 31 :d2 20 :s2 40  ;; Harmonic thickness
        ;; :m3 1 :l3 45 :a3 31 :d3 15 :s3 60  ;; Warmth
        ;; :m4 1 :l4 0  :a4 31 :d4 25 :s4 10

        :m1 1 :l1 30 :a1 31 :d1 20 :s1 20
        :m2 1 :l2 35 :a2 31 :d2 25 :s2 30
        :m3 1 :l3 40 :a3 31 :d3 25 :s3 40
        :m4 1 :l4 0  :a4 31 :d4 31 :s4 10

        })

(ploop :x
       {:type :note
        :instrument ym2612-m
        :degree [5 1 1]
        })

(stop)
(ploop :x
       {:type :note
        :instrument ym2612-m
        :chord-size 4
        #_#_:freq [200 400]
        #_#_:midinote [60 61 62]
        ;; :note [:c :d :e]
        :mode :mixolydian
        :degree [5 1 1]
        :amp [1 0.7 0.7]}
       {:quant 6})

(stop)
(ploop :x
       {:instrument ym2612-m

        :l3 20
        }
       {:quant 12})

(apply +  [1/4 1/4 1/2 1/2 2 1/2
           1/2 1/2 1/3 1/3 1/3 1 1
           1/4 1/4 1/2 1/2 1/2 1 1])

(stop)
(util/keyboard-insts ym2612-m)

(util/midi-ctl :x #'ym2612-m  {21 :l4
                               22 :l3
                               23 :l2
                               24 :l1
                               25 :m4
                               26 :m3
                               27 :m2
                               28 :fb
                               })

()
(ctl)
(ctl! ym2612-m :m1 0 :m2 2 :m3 2 :m4 1)

(util/param ym2612-m :l3)
(event-debug-off)

(event :note :instrument ym2612-m :midinote 57)
(ploop :x
       {:type :note
        :instrument ym2612-m
        :midinote [57 :_]
        :amp 0.5
        :dur [1/4 3/4
              1/4 3/4
              4/4 2/4
              1/4 3/4
              2/4 2/4
              1/8 3/8
              1/4 3/4
              1/4 3/4
              ]
        }
       {:quant 8})
(stop)
(*clock* :bpm 140)


(def close-hihat (freesound 802))

(ploop :hat
       {:type :note
        :instrument close-hihat
        :dur 1
        :amp 0.5

        }
       {:quant 8})

(apply + [1/4 3/4
          1/4 3/4
          3/4 3/4
          1/4 3/4
          1/4 3/4
          1/8 3/8
          1/4 3/4
          1/4 3/4
          ])

(count '[alg fb
         m1 dt1 l1 rs1 a1 d1 sr1 r1 s1 ssg1
         m2 dt2 l2 rs2 a2 d2 sr2 r2 s2 ssg2
         m3 dt3 l3 rs3 a3 d3 sr3 r3 s3 ssg3
         m4 dt4 l4 rs4 a4 d4 sr4 r4 s4 ssg4

         ])

0x29

(let [[alg fb
       m1 dt1 l1 rs1 a1 d1 sr1 r1 s1 ssg1
       m2 dt2 l2 rs2 a2 d2 sr2 r2 s2 ssg2
       m3 dt3 l3 rs3 a3 d3 sr3 r3 s3 ssg3
       m4 dt4 l4 rs4 a4 d4 sr4 r4 s4 ssg4
       chk
       ] (with-open [out (java.io.ByteArrayOutputStream.)
                     in  (io/input-stream "/home/arne/tmp/emerald_hill_2.tfi")]
           (io/copy in out)
           (.toByteArray out))]
  [[:alg alg :fb fb]
   [:l1 l1 :m1 m1 :a1 a1 :d1 d1 :s1 s1 :r1 r1 :dt1 dt1]
   [:l2 l2 :m2 m2 :a2 a2 :d2 d2 :s2 s2 :r2 r2 :dt2 dt2]
   [:l3 l3 :m3 m3 :a3 a3 :d3 d3 :s3 s3 :r3 r3 :dt3 dt3]
   [:l4 l4 :m4 m4 :a4 a4 :d4 d4 :s4 s4 :r4 r4 :dt4 dt4]]
  )
[[:alg 5 :fb 7]
 [:l1 25 :m1 1 :a1 14 :d1 8 :s1 1 :r1 15 :dt1 1]
 [:l2 49 :m2 3 :a2 13 :d2 5 :s2 2 :r2 15 :dt2 3]
 [:l3 44 :m3 4 :a3 12 :d3 5 :s3 2 :r3 15 :dt3 6]
 [:l4 19 :m4 2 :a4 13 :d4 5 :s4 2 :r4 15 :dt4 0]]

(ym2612-mode0 (midi->hz 46) ;; Play a low note
              ;; :fb 1
              ;; :l1 37 :m1 9 :a1 31 :d1 18 :s1 2 :r1 15 :dt1 0.05 dt3 maps to small pos
              ;; :l2 19 :m2 0 :a2 31 :d2 10 :s2 2 :r2 15 :dt2 -0.05 ;; dt6 maps to neg
              ;; :l3 48 :m3 0 :a3 31 :d3 14 :s3 2 :r3 15 :dt3 0.0
              ;; :l4 12 :m4 0 :a4 31 :d4 10 :s4 2 :r4 15 ;;:dt4 0.05
              )

(stop)

(ym2612-mode0 (midi->hz 60)
              :fb 1
              :l1 37 :m1 9 :a1 31 :d1 18 :s1 2 :r1 15 :dt1 0.05
              :l2 19 :m2 0 :a2 31 :d2 10 :s2 2 :r2 15 :dt2 -0.05 ;; dt6 maps to neg
              :l3 48 :m3 0 :a3 31 :d3 14 :s3 2 :r3 15 :dt3 0.0
              :l4 12 :m4 1 :a4 31 :d4 10 :s4 2 :r4 15 ;;:dt4 0.05
              )

(ym2612-mode4 (midi->hz 60)
              :fb 2
              :l1 25 :m1 1 :a1 15 :d1 0 :s1 0 :r1 15 :dt1 -0.05
              :l2 41 :m2 2 :a2 15 :d2 0 :s2 0 :r2 15
              :l3 19 :m3 2 :a3 14 :d3 2 :s3 0 :r3 15 :dt3 0.05
              :l4 29 :m4 3 :a4 13 :d4 2 :s4 0 :r4 15 :dt4 0.05)

(util/ctl! ym2612-mode4
           :fb 7
           :l1 50 :m1 1 :a1 15 :d1 0 :s1 0 :r1 15 :dt1 -0.05
           :l2 20 :m2 2 :a2 15 :d2 0 :s2 0 :r2 15
           :l3 19 :m3 2 :a3 14 :d3 2 :s3 0 :r3 15 :dt3 0.05
           :l4 0  :m4 3 :a4 13 :d4 2 :s4 0 :r4 15 :dt4 0.05)
(stop)

(util/keyboard-insts ym2612-mode4)
