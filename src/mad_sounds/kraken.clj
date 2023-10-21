(ns mad-sounds.kraken
  (:require
   [overtone.live :refer :all]
   [vibeflow.midi.core :as midi]
   [vibeflow.midi.jack :as jack]))

(defonce client (jack/client "kraken"))
(defonce midi-in (jack/midi-in-port client :in))

(definst gong [note 60 duration 2 gate 1]
  "Super simple synth, just a single sine wave, but with an envelope like that of a gong"
  (let [freq (midicps note)
        env (env-gen (adsr 0.002 (- duration 0.002) 0.001 0.1) :gate gate)]
    (* env
       (sin-osc freq))))

(definst out-of-phase-saws [note 60 lpf-factor 2.0 gate 1]
  (let [freq (midicps note)]
    (* (env-gen (adsr) gate)
       (rlpf
        (mix [(saw (+ (* 2 (lf-tri 19)) freq))
              (* (sin-osc 15)
                 (saw (+ 3 (* 2 (lf-tri 5)) freq)))])
        (* freq lpf-factor)))))

(defmulti handle (fn [[chan event _ _]] [chan event]))
(defmethod handle :default [e] (prn 'unhandled e))

(defmethod handle [0 :cc] [[_ _ b1 b2]]
  ;; Orca sends this when you stop the clock, kill all sound
  (when (= [123 0] [b1 b2])
    (stop)))

(defmethod handle [0 :note-on] [[_ _ note]]
  (gong note))

(defmethod handle [1 :note-on] [[_ _ note]]
  (ctl out-of-phase-saws :gate 0)
  (out-of-phase-saws :note note))

(defmethod handle [1 :cc] [[_ _ cc v]]
  (when (= 64 cc)
    (ctl out-of-phase-saws :lpf-factor (/ v 8))))

(comment
  (jack/register client :process ::monitor
                 (fn [client frames]
                   (when-let [events (seq (jack/read-midi-events midi-in))]
                     (doseq [[e t] events]
                       (handle  (midi/event e))))
                   true))
  (stop))

;; ............................
;; .D8................1........
;; ..:12O........2C4.R.R.......
;; .............D234T65mF......
;; .............*!10F..........
;; ............................
;; ............................
;; ............................
;; ............................
;; ............................
;; ..4C4.......................
;; .aV3.....Va.................
;; .........3C8................
;; .........528TIKMOQOMK.......
;; .........Va.M...............
;; ........D3..J...............
;; ........*:02M...............
;; ............................
;; ............................
