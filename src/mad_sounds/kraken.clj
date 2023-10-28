(ns mad-sounds.kraken
  (:require
   [overtone.live :refer :all]
   #_[overtone.inst.synth :as ois]
   [overtone.inst.drum :as drum]
   [vibeflow.midi.core :as midi]
   [vibeflow.midi.jack :as jack]))

(defonce client (jack/client :kraken))
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

(definst swinu4 [note 60 gate 1]
  (let [freq (midicps note)
        fm   (sin-osc 7)
        env  (env-gen (adsr 0.01 0.25 0.3 0.3) :gate gate)
        saw-signal (saw (mul-add fm 5 freq))
        sin-signal (sin-osc [freq (+ 3 freq)])]
    (g-verb
     :roomsize 3
     :revtime 0.5
     :in (* env
            (mix [(lpf saw-signal 8000)
                  (/ sin-signal 2)
                  (/ (lf-noise0) 12)])))))

(defmulti handle (fn [[chan event _ _]] [chan event]))
(defmulti handle-cc (fn [[chan _ cc _]] [chan cc]))

(defmethod handle :default [e] (prn 'unhandled e))
(defmethod handle-cc :default [e] (prn 'unhandled-cc e))

(defmethod handle-cc [15 123] [[_ _ b1 b2]]
  (when (= 0 b2)
    ;; Orca sends this when you stop the clock, kill all sound
    (stop)))

(defmethod handle [0 :note-on] [[_ _ note]] (ctl gong :gate 1 :note note))
(defmethod handle [0 :note-off] [[_ _ note]] (ctl gong :gate 0))

(defmethod handle [1 :note-on] [[_ _ note]] (ctl out-of-phase-saws :note note :gate 1))
(defmethod handle [1 :note-off] [[_ _ note]] #_(ctl out-of-phase-saws :gate 0))
(defmethod handle-cc [1 64] [[_ _ cc v]] (ctl out-of-phase-saws :lpf-factor (/ v 8)))

(defmethod handle [2 :note-on] [[_ _ note]] (ctl swinu4 :note note :gate 1))
(defmethod handle [2 :note-off] [[_ _ note]] (ctl swinu4 :gate 0))

;; (defmethod handle [4 :note-on] [[_ _ note]] (ctl pw-gong :note note :gate 1))
;; (defmethod handle [4 :note-off] [[_ _ note]] (ctl pw-gong :gate 0))
;; Volume, see Open Stage Control file
(defmethod handle-cc [0 7] [[_ _ _ v]] (inst-volume! gong (/ v 128.0)))
(defmethod handle-cc [1 7] [[_ _ _ v]] (inst-volume! out-of-phase-saws (/ v 128.0)))
(defmethod handle-cc [2 7] [[_ _ _ v]] (inst-volume! swinu4 (/ v 128.0)))

;; drums
(defmethod handle [10 :note-on] [[_ _ note]]
  (case note
    35 (drum/kick)
    38 (drum/snare)
    42 (drum/closed-hat)
    46 (drum/open-hat)
    nil))

(defmethod handle [10 :note-off] [[_ _ note]])

(jack/connect!
 #{["Overtone:out_1" "Built-in Audio Analog Stereo:playback_FL"]
   ["Overtone:out_2" "Built-in Audio Analog Stereo:playback_FR"]
   ["orca:out" "kraken:in"]})

(comment

  (jack/register client :process ::monitor
                 (fn [client frames]
                   (when-let [events (seq (jack/read-midi-events midi-in))]
                     (doseq [[e t] events]
                       (let [[_ et :as me] (midi/event e)]
                         (prn me)
                         (if (= :cc et)
                           (handle-cc me)
                           (handle me)))))
                   true))

  (kill gong)
  (kill out-of-phase-saws)
  (kill swinu4)
  (do
    (gong)
    (out-of-phase-saws)
    (swinu4)
    (gong))

  (handle [1 :note-on 36 127])


  (stop))
