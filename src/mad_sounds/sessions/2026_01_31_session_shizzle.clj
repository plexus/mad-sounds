(ns mad-sounds.sessions.2026-01-31-session-shizzle
  (:require
   [overtone.live :refer :all]
   [vibeflow.session :as session]
   [vibeflow.util :as util]))

(session/open! "2026-01-31-session-shizzle")

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
  (let [env1 (adsr (lin-exp a1 0 31 0.001 10.0)
                   (lin-exp d1 0 31 0.001 10.0)
                   (lin-lin s1 0 127 0.0 1.0)
                   (lin-exp r1 0 31 0.001 10.0))
        env2 (adsr (lin-exp a2 0 31 0.001 10.0)
                   (lin-exp d2 0 31 0.001 10.0)
                   (lin-lin s2 0 127 0.0 1.0)
                   (lin-exp r2 0 31 0.001 10.0))
        env3 (adsr (lin-exp a3 0 31 0.001 10.0)
                   (lin-exp d3 0 31 0.001 10.0)
                   (lin-lin s3 0 127 0.0 1.0)
                   (lin-exp r3 0 31 0.001 10.0))
        env4 (adsr (lin-exp a4 0 31 0.001 10.0)
                   (lin-exp d4 0 31 0.001 10.0)
                   (lin-lin s4 0 127 0.0 1.0)
                   (lin-exp r4 0 31 0.001 10.0))
        m1'  (+ m1 (* 0.5 (< m1 1))) ;; 0=0.5, higher -> 1=1
        m2'  (+ m2 (* 0.5 (< m2 1)))
        m3'  (+ m3 (* 0.5 (< m3 1)))
        m4'  (+ m4 (* 0.5 (< m4 1)))
        op1  (* (dbamp (* -0.75 (- 127 l1))) pm-scale
                (fb-sine-n (* (+ m1' dt1) freq) (* fb (/ Math/PI 7.0)))
                (env-gen env1 gate))
        op2  (* (dbamp (* -0.75 (- 127 l2))) pm-scale (sin-osc (* (+ m2' dt2) freq) op1) (env-gen env2 gate))
        op3  (* (dbamp (* -0.75 (- 127 l3))) pm-scale (sin-osc (* (+ m3' dt3) freq) op2) (env-gen env3 gate))
        op4  (* amp (dbamp (* -0.75 (- 127 l4))) (sin-osc (* m4' freq) op3) (env-gen env4 gate :action FREE))]
    op4))

(session/defsessinst x2 ym2612-mode0)

(db->amp (* -0.75 0))

(x1)
(stop)


(util/keyboard-insts x2)
(util/midi-ctl ::x1 x2 {0 :l4
                        1 :l3
                        2 :l2
                        3 :l1

                        5 :a4
                        6 :d4
                        7 :s4
                        8 :r4

                        9 :a3
                        10 :d3
                        11 :s3
                        12 :r3
                        })

(filter #(= "l1" (:name %)) (:params x1))

(stop)
