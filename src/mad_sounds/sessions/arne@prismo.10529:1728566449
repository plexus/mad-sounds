(ns mad-sounds.sessions.richmond-2024-10
  (:require
   [vibeflow.shuttlexpress :as shex]
   [casa.squid.jack :as jack]
   [overtone.live :refer :all]))

(doseq [[from to](jack/connections)]
  (jack/disconnect from to))

(jack/ports)
(jack/connect!
 #{["Overtone:out_1"
    #{"AG06/AG03 Analog Stereo:playback_FL"
      "AG06/AG03 Digital Stereo (IEC958):playback_FL"
      "AG06/AG03 Pro:playback_AUX0"
      "Family 17h/19h HD Audio Controller Pro:playback_AUX0"
      "Family 17h/19h HD Audio Controller Speaker + Headphones:playback_FL"
      "oso:in"}]
   ["Overtone:out_2"
    #{"AG06/AG03 Analog Stereo:playback_FR"
      "AG06/AG03 Digital Stereo (IEC958):playback_FR"
      "AG06/AG03 Pro:playback_AUX1"
      "Family 17h/19h HD Audio Controller Pro:playback_AUX1"
      "Family 17h/19h HD Audio Controller Speaker + Headphones:playback_FR"
      "oso:in"}]



   [#{"AG06/AG03 Analog Stereo:capture_FL"
      "Family 17h/19h HD Audio Controller Pro:capture_AUX0"
      #_"Family 17h/19h HD Audio Controller Digital Microphone:capture_FL"
      "Family 17h/19h HD Audio Controller Headphones Stereo Microphone:capture_FL"
      }
    "Overtone:in_1"]
   [#{"AG06/AG03 Analog Stereo:capture_FR"
      "Family 17h/19h HD Audio Controller Pro:capture_AUX1"
      #_"Family 17h/19h HD Audio Controller Digital Microphone:capture_FR"
      "Family 17h/19h HD Audio Controller Headphones Stereo Microphone:capture_FR"}
    "Overtone:in_2"]
   })

(jack/connections)

(demo (out [0 1](sin-osc)))

(defsynth vocoder [freq 200
                   q 20]
  (let [bands 30
        bpfhz   (map (fn [band]
                       (long
                        (+ 50
                           (* band (/ 16000 bands)))))
                     (range bands))
        ;; carrier
        carrier (+ (var-saw freq :width 0)
                   (comb-l (pink-noise) 1/20 (/ 1 freq) 3))
        in (sound-in [0 1])
        mod     (mix (compander in in 0.1 10 1))
        bpfmod  (* (bpf mod bpfhz (/ 1 q)) (sqrt q))
        track   (lag3 (amplitude bpfmod) 0.03)
        bpfcar  (* (bpf carrier bpfhz (/ 1 q)) (sqrt q) track)]
    (out [0 1] (+ (lpf (hpf mod 2000) 14000) (* 4 bands (mix bpfcar))))))

(def v (vocoder))
(kill vocoder)
(ctl v :q 1)
(ctl v :freq (midi->hz 60))

(demo
  5
  (let [sig (sound-in [0 1])]
    sig))

(pclear)

(pplay :vocoder-mod
       (repeat
        (pbind {:type :ctl
                :instrument v
                :octave 4
                :dur [2 1]
                :note [:c :d :e :f :g :e]})))



(def snare (freesound 26903))
(def kick (freesound 2086))
(def close-hihat (freesound 802))
(def open-hihat (freesound 26657))
(def stick (freesound 628568))

(map sample?
     [
      (freesound  628568)
      (sample (freesound-path 628568))])
(class  stick)
;; =>
(*clock* :bpm 110)
kick
(stop)

(definst pad [freq 300 amp 1 gate 1]
  (let [freq (for [i (range 4)]
               (* freq (rand-in-range 1 1.03)))
        mix (lin-lin (sin-osc 0.5) -1 1 0 1)]
    (splay (* amp
              (env-gen (adsr 0.2 0.1 1 0.3) :gate gate :action FREE)
              (+ (* mix (hpf (var-saw (lin-lin (sin-osc 4)
                                               -1 1
                                               (/ freq 1.02)
                                               (* freq 1.02)))
                             (* 6 freq)))
                 (* (- 1 mix) (lpf (square freq) (* 3 freq))))))))


(def pad-patt [0 0 3 -1])
(def pad-patt [0 0 3 5])
(ploop :pad {:note (pdo pad-patt)
             :dur 4
             :instrument pad})

(def tinkle-pat (shuffle (range 12)))

(ploop :tinkle {:note (pdo tinkle-pat)
                :octave 6
                :dur 1/4
                :instrument pad})
(pad)
(stop)
(clear)
(pad)

(def semitone (/ (midi->hz 2)
                 (midi->hz 1)))

(do

  (ploop :hat
         {:instrument close-hihat
          :dur [1/2]
          :amp (interleave (pwhite 0.4 0.7)
                           (pwhite 0.6 0.9))})

  (ploop :snare
         {:instrument snare
          :note (pchoose [[0 :- :- :-
                           :- :- 0  :-
                           0 :- 0 :-
                           0 :- :- :-]
                          [0 :- :- :-
                           :- :- :-  :-
                           :- 0 :- :-
                           0 :- :- :-]])
          :dur 1/4
          :amp (pwhite 0.7 0.9)})

  (ploop :kick
         {:instrument kick
          :note [0 :- :- :-
                 0 :-  :- :-
                 0 :-  :- 0
                 :-  :- 0 :-
                 ]
          :dur 1/4
          :amp (pwhite 0.4 0.6)
          })

  (ploop :_stick
         {:instrument stick
          :note [0 :- :- :-
                 :- 0 :- :-
                 0 :- :- :-
                 :- :- 0 :-]
          :dur [1/4]
          :amp 0.2})
  )

(require '[overtone.synth.stringed :refer :all])
(require '[overtone.inst.sampled-trumpet :refer :all])
(stop)
(params-vec {:instrument sampled-trumpet
             })

(sampled-trumpet 62)

(def g (guitar))
(guitar-strum g :E 0.5)
(guitar-strum g :A 0.5)
(guitar-strum g :C 0.5)
(guitar-strum g :G 0.5)
(guitar-strum g :D :down 1)
(stop)
(params-vec  {:instrument kick
              :note [0 :_ 0 :-]
              :dur [1 3/2 1/2 1]})

(stereo-partial-player :buf 1)

(into {} snare )
(:id snare)
(ppause :snare)
(stop)

(sample-player snare)
(into {} wolves)
(def wolves (sample "~/Downloads/mixkit-wolves-at-scary-forest-2485.wav"))

(definst scrubbery [buf-id 0
                    pos 1000
                    depth 500
                    freq 1
                    ]
  (buf-rd 2 buf-id #_(phasor:ar :start 0 :end pos)
          (lin-lin (var-saw freq)
                   -1 1
                   (- pos depth) (+ pos depth))))

(definst slidinator [buf-id 0
                     depth 500
                     freq 1
                     start 0
                     end 1
                     amp 1
                     gate 1]
  (let [samples (buf-samples buf-id)
        start (+ depth (* start samples))
        end  (* end (- samples depth))
        dur (/ (- end start) (sample-rate))
        pos (phasor:ar :start start :end end)]
    (* amp
       (env-gen (envelope [0 1 0] [0.01 dur] :linear 2) gate :action FREE)
       (env-gen (envelope [0 1 1 0] [0.01 (- dur 0.02) 0.01]))
       (buf-rd 2 buf-id (lin-lin (var-saw freq)
                                 -1 1
                                 (- pos depth) (+ pos depth))))))

(definst butcher [buf-id 0
                  depth 500
                  freq 1
                  start 0
                  end 1
                  speed 1
                  ]
  (let [samples (buf-samples buf-id)
        start (+ depth (* start samples))
        end  (* end (- samples depth))
        pos #_(phasor:ar :start depth :end (- (buf-samples buf-id) depth))
        (lin-lin (var-saw (* speed (/ 1
                                      (/ (- end start)
                                         (sample-rate)))))
                 -1 1
                 depth (- (buf-samples buf-id) depth))]
    (buf-rd 2 buf-id (lin-lin (var-saw freq)
                              -1 1
                              (- pos depth) (+ pos depth)))))

(slidinator wolves
            :freq 10
            :depth 1000
            )

(ctl slidinator :gate 0)


(ploop :wolves
       {:instrument slidinator
        :buf-id wolves
        :degree [1 2]
        :mode :mixolydian
        :start 0.1  #_(range 0 0.3 0.1)
        ;; :end 0.4 #_(range 0.7 1 0.1)
        :dur 8
        :octave 2
        }
       )

(eget   {:type :ctl
         :instrument slidinator
         :depth [800 900 1000]}
        :freq)
(stop)

(
 (:freq
  @#'overtone.studio.event/event-derivations)
 {:type :ctl})
(some #(contains? {} %) [:midinote :note :degree])

(ploop :wolves-ctl
       {:type :ctl
        :instrument slidinator
        :depth [800 900 1000 1100 1000 900]
        :dur [1/4]
        :amp (reverse (range 0 0.5 0.1))}
       )

(stop)
(ctl slidinator :depth 5000)
(ctl slidinator :freq 400)
(ctl slidinator :start 0.6)
(ctl slidinator :end 0.7 )
(ctl slidinator :speed 1 )
(stop)
(ctl slidinator :depth 2000)
(ctl slidinator :freq 200)

(scrubbery (to-sc-id wolves)
           (/ (:size wolves) 2)
           :freq 100)
(event-debug-off)
(reset! (:value (synth-param scrubbery :pos))
        (/ (:size wolves) 2))

(ctl scrubbery :pos 2001)

(kill slidinator)

(shex/activate-shuttlexpress-events)

(defn synth-param [synth param-name]
  (some #(when (#{param-name (name param-name)} (:name %))
           %) (:params synth)))

(on-event :shuttlexpress/wheel
          (fn [{:keys [change]}]
            (let [param (synth-param scrubbery :pos)]
              (ctl scrubbery :pos (swap! (:value param) + change)))
            )
          ::scrub)


(definst subtractive [freq 440]
  (lpf (pulse :freq freq :width 0.5)
       (line :start 8000 :end 660 :dur 2)))

(subtractive :freq 200)
(stop)
(free )

(meta #'foo)
(pp-node-tree)

(defn foo [])


(def b (buffer 4096))

(use 'overtone.studio.util)

(defonce wave-bus-synth (bus->buf [:after (foundation-monitor-group)] 0 b))
(bus->buf )

(buffer-data b)
