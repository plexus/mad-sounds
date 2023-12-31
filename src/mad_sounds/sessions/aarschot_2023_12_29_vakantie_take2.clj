(ns mad-sounds.sessions.aarschot-2023-12-29-vakantie-take2
  (:require
   [overtone.live :refer :all]
   [vibeflow.freesound :refer [fsample]]
   [vibeflow.util :as util :refer [defloop]]
   [vibeflow.controller-ui :as cui]
   #_:reload))

(definst whomp [note {:default 60 :max 85}
                cutoff {:min 0 :default 55 :max 100}
                mod-freq {:min 0 :default 1.2 :max 6}
                mod-depth {:min 0 :default 0.5 :max 1}
                release {:min 0 :default 0.1 :max 1}
                resonance {:min 0 :default 14 :max 100}
                phase {:min 0 :default 0 :max 2}
                gate 1]
  (let [freq (midicps note)
        freq2 (+ freq (* freq (lf-tri mod-freq :iphase phase) mod-depth))]
    (* (env-gen (asr :release release) :gate gate :action FREE)
       (rlpf
        (square freq2)
        (* freq2 (+ 1 (* cutoff 3/100)))
        :rq (/ 1 (+ 1/10000 resonance))))))

(definst pad [note {:default 60 :max 85}
              release {:min 0 :default 0.5 :max 1}
              mod-depth {:min 0 :default 0.71 :max 10}
              mod-freq {:min 0 :default 7.48 :max 10}
              gate 1]
  (let [freq (midicps note)
        mod (sin-osc (* (+ 1 (* mod-depth (sin-osc (* freq (/ 1 (- 10 mod-freq)))))) freq))]
    (* (env-gen (asr :release release) :gate gate :action FREE)
       (rlpf
        (sin-osc freq :phase mod)
        :freq (* 1.5 freq)))))

(cui/ctl-synth! #'pad)
(util/midi-ctl :whomp #'pad)

(defloop hats 1 [m b]
  (let [s (fsample :hihat-closed2)]
    (at (m (+ 0   b)) (s :amp (+ 0.8 (rand))))
    (at (m (+ 0.5 b)) (s :amp (+ 0.7 (rand))))))

(defloop kicks 4 [m b]
  (let [s (fsample :kicky)]
    (at (m (+ 0   b)) (s :amp (+ 1)))
    #_(at (m (+ 0.5   b)) (s :amp (+ 1)))
    ;; (at (m (+ 1.75 b)) (s :amp (+ 0.8 (rand))))
    ;; (at (m (+ 2.25 b)) (s :amp (+ 0.8 (rand))))
    #_(at (m (+ 2.5  b)) (s :amp (+ 0.8 (rand))))
    (at (m (+ 1.75 b)) (s :amp (+ 0.8 (rand))))
    (at (m (+ 2.5 b)) (s :amp (+ 1)))
    ))

(defloop snares 4 [m b]
  (let [s (fsample :snare-bright)]
    (at (m (+ 1 b)) (s :amp (+ 1)))
    (at (m (+ 3 b)) (s :amp (+ 1)))))

(defloop whomper 16 [m b]
  (let [b #(m (+ %1 (* %2 1/4 ) b))]
    (at (b 0 0) (whomp :note 40))
    (at (b 0 2) (whomp :note 47))
    (at (b 1 0) (whomp :note 44))
    (at (b 1 3) (ctl whomp :gate 0))

    (at (b 4 0) (whomp :note 49))
    (at (b 4 2) (whomp :note 56))
    (at (b 5 0) (whomp :note 52))
    (at (b 5 2) (ctl whomp :gate 0))

    (at (b 8 0) (whomp :note 47))
    (at (b 8 2) (whomp :note 51))
    (at (b 9 0) (ctl whomp :gate 0))

    (at (b 12 0) (whomp :note 45))
    (at (b 13 0) (ctl whomp :gate 0))
    ))


(def m (metronome 164))


(defn go! []
  (metro-start m 0)
  (hats m)
  (kicks m)
  (snares m)
  #_(whomper m))

;; two round buttons on novation launchkey next to the pads
(on-event [:midi :control-change] (fn [{:keys [data1 data2]}]
                                    (case [data1 data2]
                                      [108 127] (go!)
                                      [109 0] (stop)
                                      nil))
          ::stop)
scale
(cui/show!)
(cui/ctl-synth! whomp)
(cui/ctl-synth! pad)
(util/midi-preview :whomp #'pad)
(util/midi-ctl :whomp whomp)

;;;;;;;;;;;;;;;;;;;;
(comment
  (clojure.repl/apropos "tri")
  (stop)
  (event-debug-off)
  (event-debug-on)
  (m :bpm 173)

  )

(def bring-it-on-sample (sample "samples/bring-it-on.wav"))



(definst bring-it-on []
  (pan2
   (free-verb
    (scaled-play-buf 1 bring-it-on-sample :action FREE)
    :mix 0.3
    :room 0.9
    :damp 0.2)))

;; Amazing CC-0 female vocals
;; https://archive.org/details/femaleacapellavocals/A+Word+to+no+one+120bpm.wav
