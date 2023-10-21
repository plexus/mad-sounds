(ns mad-sounds.oscilators
  (:use overtone.live))


;; We use a saw-wave that we defined in the oscillators tutorial
(definst saw-wave [freq 440 attack 0.01 sustain 0.4 release 0.1 vol 0.4]
  (* (env-gen (lin attack sustain release) 1 1 0 1 FREE)
     (saw freq)
     vol))

;; We can play notes using frequency in Hz
#_(saw-wave 440)
#_(saw-wave 523.25)
#_(saw-wave 261.62) ; This is C4

;; We can also play notes using MIDI note values
#_(saw-wave (midi->hz 69))
#_(saw-wave (midi->hz 72))
#_(saw-wave (midi->hz 60)) ; This is C4

;; We can play notes using standard music notes as well
#_(saw-wave (midi->hz (note :A4)))
#_(saw-wave (midi->hz (note :C5)))
#_(saw-wave (midi->hz (note :C4))) ; This is C4! Surprised?

;; Define a function for convenience
(defn note->hz [music-note]
  (midi->hz (note music-note)))

; Slightly less to type
#_(saw-wave (note->hz :C5))

;; Let's make it even easier
(defn saw2 [music-note]
  (saw-wave (midi->hz (note music-note))))

;; Great!
#_(saw2 :A4)
#_(saw2 :C5)
#_(saw2 :C4)

;; Let's play some chords


;; this is one possible implementation of play-chord
(defn play-chord [a-chord]
  (doseq [note a-chord] (saw2 note)))

;; We can play many types of chords.
;; For the complete list, visit https://github.com/overtone/overtone/blob/master/src/overtone/music/pitch.clj and search for "def CHORD"
#_(play-chord (chord :C4 :major))

;; We can play a chord progression on the synth
;; using times:
(defn chord-progression-time []
  (let [time (now)]
    (at time (play-chord (chord :C4 :major)))
    (at (+ 2000 time) (play-chord (chord :G3 :major)))
    (at (+ 3000 time) (play-chord (chord :F3 :sus4)))
    (at (+ 4300 time) (play-chord (chord :F3 :major)))
    (at (+ 5000 time) (play-chord (chord :G3 :major)))))

#_(chord-progression-time)

;; or beats:
(defonce metro (metronome 120))
(metro)
(defn chord-progression-beat [m beat-num]
  (at (m (+ 0 beat-num)) (play-chord (chord :C4 :major)))
  (at (m (+ 4 beat-num)) (play-chord (chord :G3 :major)))
  (at (m (+ 8 beat-num)) (play-chord (chord :A3 :minor)))
  (at (m (+ 14 beat-num)) (play-chord (chord :F3 :major)))
)

#_(chord-progression-beat metro (metro))

;; We can use recursion to keep playing the chord progression
(defn chord-progression-beat [m beat-num]
  (at (m (+ 0 beat-num)) (play-chord (chord :C4 :major)))
  (at (m (+ 4 beat-num)) (play-chord (chord :G3 :major)))
  (at (m (+ 8 beat-num)) (play-chord (chord :A3 :minor)))
  (at (m (+ 12 beat-num)) (play-chord (chord :F3 :major)))
  (apply-at (m (+ 16 beat-num)) chord-progression-beat m (+ 16 beat-num) [])
)

#_(chord-progression-beat metro (metro))
(stop)
