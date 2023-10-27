(ns mad-sounds.kraken
  (:require
   [overtone.live :refer :all]
   #_[overtone.inst.synth :as ois]
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
(kill gong)
(kill out-of-phase-saws)
(kill swinu4)
(do
  (gong)
  (out-of-phase-saws)
  (swinu4)
  (pw-gong)

  )

(defmethod handle [0 :note-on] [[_ _ note]] (ctl gong :gate 1 :note note))
(defmethod handle [0 :note-off] [[_ _ note]] (ctl gong :gate 0))

(defmethod handle [1 :note-on] [[_ _ note]] (ctl out-of-phase-saws :note note :gate 1))
(defmethod handle [1 :note-off] [[_ _ note]] #_ (ctl out-of-phase-saws :gate 0))
(defmethod handle-cc [1 64] [[_ _ cc v]] (ctl out-of-phase-saws :lpf-factor (/ v 8)))

(defmethod handle [2 :note-on] [[_ _ note]] (ctl swinu4 :note note :gate 1))
(defmethod handle [2 :note-off] [[_ _ note]] (ctl swinu4 :gate 0))

(defmethod handle [4 :note-on] [[_ _ note]] (ctl pw-gong :note note :gate 1))
(defmethod handle [4 :note-off] [[_ _ note]] (ctl pw-gong :gate 0))
;; Volume, see Open Stage Control file
(defmethod handle-cc [0 7] [[_ _ _ v]] (inst-volume! gong (/ v 128.0)))
(defmethod handle-cc [1 7] [[_ _ _ v]] (inst-volume! out-of-phase-saws (/ v 128.0)))
(defmethod handle-cc [2 7] [[_ _ _ v]] (inst-volume! swinu4 (/ v 128.0)))

(comment
  (jack/register client :process ::monitor
                 (fn [client frames]
                   (when-let [events (seq (jack/read-midi-events midi-in))]
                     (doseq [[e t] events]
                       (let [[_ et :as me] (midi/event e)]
                         (if (= :cc et)
                           (handle-cc me)
                           (handle me)))))
                   true))
  (stop))

;; #.Channel.0...pad...bass.
;; #.note.........lpf.freq..
;; ..zC4....................
;; .DC14TOMQK.........1.....
;; ..:12M........2C4........
;; .............D234T369z...
;; .............*!10z.......
;; .........................
;; #.Channel.0...gong.......
;; #.speed.control...melody#
;; ..2C4....................
;; .aV3.....Va..............
;; .........3C8.............
;; .........528TIKMOQOMK....
;; .........Va.M............
;; ........D3..J............
;; ........*:02M............
;; .........................
;; #...............#........
;; #..4C6..........#........
;; #...1B3.........#........
;; #.0D424TFCAa....#........
;; #...:42.........#........
;; #...............#........
;; .........................
;; ..Cc.....................
;; .76B5....................
;; ...1XE...................
;; .....E.....:23E..........
;; ....E.E......:23A........
;; .......E......:23A.......
;; ........E...:23C.........
;; .........E.:23A..........
;; ..........E.E:23G........
;; ..........*:23F..........
;; .........1...............
