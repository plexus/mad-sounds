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

;;; Combining Sounds

;; Doubling oscillators

(demo (+ (var-saw 300 :width 0)
         (var-saw 300 :width 0 :iphase 0.15)))

;; more cancellation = thinner/tinnier sound
(demo (+ (var-saw 300 :width 0)
         (var-saw 300 :width 0 :iphase 0.45)))

;; two saws at half phase is saw of 2x frequency
(demo (+ (var-saw 300 :width 0)
         (var-saw 300 :width 0 :iphase 0.5)))


;; Oscillator detuning

(demo
  10
  (let [oscillators 6
        detune (mouse-x :min 0.00001 :max 0.3)
        freq 300]
    (mix
     (for [i (range oscillators)]
       (var-saw (* freq (+ 1 (* detune (- i (/ oscillators 2)))))
                :width 0
                :iphase (rrand 0 1)))))
  )


;; Layering oscillators

(demo ;; saw + square
  (moog-ff
   (+ (* 0.5 (var-saw :width 0))
      (square))
   5000))


(demo ;; two squares, octave apart
  (* (env-gen (perc))
     (moog-ff
      (+ (square 150)
         (* 0.75 (square 300 #_(lin-lin (sin-osc 75) 0 1 299 301))) ;; some detuning?
         #_(* 0.5 (square 600))) ;; add a third?
      5000)))


(demo ;; saw + square, drop square an octave
  (+ (var-saw 300 :width 0)
     (square 150)))

;; Hard synching oscillators

;; seems this is actually not that easy with SC/Overtone, there's sync-saw which
;; is the main built in option

;; https://scsynth.org/t/advanced-synthesis-oscillator-sync/239/7

(demo 10 (sync-saw 100 (mouse-x 200 299)))

;; In practice
;; Some more listening exercises

;; Different mixes of saw + square
(demo 5
      (+ (* (mouse-x) (saw 300))
         (* (mouse-x 1 0) (square 300))))


;; different intervals
(demo 5
      (let [semtones (round (mouse-x 0 12) 1)]
        (+ (* (mouse-y) (saw (* 300 (midiratio semtones))))
           (* (mouse-y 1 0) (square 300)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chapter 8

;; basic bass

(definst bass [freq 70
               brightness 1.1
               attack 0.1
               decay 0.1
               sustain 0.7
               release 0.1
               amp 0.5
               gate 1
               lag 0.2]
  (let [freq (lag:kr freq lag)
        env  (env-gen (adsr attack decay sustain release)
                      :gate gate)]
    (-> (var-saw freq :width 0)
        (rlpf (* brightness freq env))
        (* amp env))))

(pplay
 ::bass
 (pbind {;;:type ^{:dur [1/2 7/2]} [:note :ctl]
         :degree [:i :i :iii :ii :i :iv :v]
         :dur    [1/2 1 1/2 1/2 1/2 1/2 1/2]
         :root   ^{:dur 8} [:c3 :d3 :b2 :c3]
         }
        ##Inf)
 {:proto {:instrument #'bass
          :attack     0.01
          :octave     3
          :lag        2
          :amp        0.5
          :brightness 1.1}})

(ppause ::bass)

(bass :lag 1 :amp 0.5)
(ctl bass :freq 100)
(stop)
(lag-ud)
