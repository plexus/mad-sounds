(ns mad-sounds.sessions.how-to-make-a-noise
  "Following along with the book 'How to make a noise: Analog Synthesis'"
  (:use overtone.live))

;;;; Chapter 2. wave forms

;;; Sawtooth

;; - bright, harsh, should be filtered
;; - brass
;; - string
;; - general "fat" synth sounds, stabs and basses

;; multiple options

(demo (saw)) ;; band-limited?
(demo (lf-saw)) ;; not band limited (more likely to alias?)

;; carefully, by default var-saw creates a triangle wave! change :width to 0 or
;; 1 to get a saw

(demo (var-saw :width 0)) ;; downward slopw
(demo (var-saw :width 1)) ;; upward slope

;; typically filtered

(demo (lpf (var-saw :width 1)
           (line 500 2500 2)))

;; Run through hpf to clean up a mix (less muddy lower frequency range)

(demo (hpf (lpf (var-saw :width 1)
                (line 500 2500 2))))

;;; Square and Pulse

;; - hollow
;; - reedy, wooden
;; - woodwind
;; - bass, on its own or to fatten up the sound
;; - sub-oscillator (below base frequency)

(demo (square 300))

;; pulse
;; as pulse gets closer to 0/1, becomes thinner, more nasal, more noisy

(demo (pulse 300 :width 0.5))
(demo (pulse 300 :width 0.3))
(demo (pulse 300 :width 0.1))
(demo (pulse 300 :width 0.02))
(demo (pulse 300 :width 0.01))
(demo (pulse 300 :width (line 0 1 2)))

(demo (lpf (mix (square [300 302]))
           1000))
(demo (lpf (mix (pulse [300 302] :width 0.3))
           1000))

;; PWM

(demo (lpf (pulse 300 :width (lin-lin (var-saw 300) 0 1 0.45 0.55))
           1000))

;;; Sine

;; - vary basic sound
;; - no harmonics, no use filtering (only reduces volume)
;; - can be used to thicken patches - add depth/roundness/fullness
;; - can be added to bass - careful not to blow up your speakers!
;; - can be approximated by a triangle wave, often was on old synths

(demo (sin-osc))

;;; Triangle

;; - less reedy/sharp than a square
;; - or a sharper sine-wave

;; ! var-saw with width=0.5 (the default) yields a triangle !

;; Listen how it "sits between" the sine wave and the square

(demo (sin-osc 300))

(demo (var-saw 300))

(demo (square 300))


;;; Noise

;; - unpitched sounds
;; - wind/breath (e.g. in a flute)
;; - percussion

;; Comes in a variety of colors, plus some other options

(demo (white-noise)) ;; equal enery at all frequencies
(demo (gray-noise))  ;; random sequence of impulses based on flipping bits
(demo (pink-noise))  ;; 3db/octave lpf
(demo (brown-noise)) ;; 6db/octave lpf
(demo (clip-noise))  ;; random sequence of -1 or 1
(demo (crackle))     ;; based on chaotic function

;; Not sure about these
(demo (lf-noise0))
(demo (lfd-noise0))
(demo (hasher (sin-osc)))
