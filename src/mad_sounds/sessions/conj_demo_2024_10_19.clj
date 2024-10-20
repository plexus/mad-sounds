(ns mad-sounds.sessions.conj-demo-2024-10-19
  (:use overtone.live
        overtone.inst.synth
        overtone.inst.drum
        overtone.inst.sampled-piano
        overtone.inst.sampled-trumpet
        )
  (:require
   [casa.squid.plasticine :as p]
   [quil.core :as q]
   [vibeflow.synths :as s]
   [vibeflow.studio :as ui]
   [vibeflow.freesound :as f]))

(demo (pan2 (sin-osc)))

(def snare (freesound 26903))
(def kick (freesound 2086))
(def close-hihat (freesound 802))
(def open-hihat (freesound 26657))
(def stick (freesound 628568))

(kick2)
(stop)
(ploop :ch {:instrument close-hihat})
(ploop :oh {:instrument open-hihat
            :note [:- 0]
            :dur 1/2
            })
(ploop :tr {:instrument sampled-piano
            :degree (shuffle (range 7))
            :dur [1]
            :amp 0.1})
(stop)
(ploop :arp
       {:type :chord
        :instrument sampled-trumpet
        :attack 0.01
        :decay 0.01
        :sustain 0.7
        :degree [:i :i :iv :iv :v :v :i :i]
        :amp (pwhite 0.7 0.9)
        :dur 2
        :octave [5]
        :strum 1/2}
       {:quant 16})

(ploop :p {:type :chord
           :chord-size 3
           :instrument sampled-piano
           :degree [:i :iv :v :i]
           :octave 4
           :amp 0.2
           :dur 4}
       {:quant 16})

(def wolves (freesound 519592))
(def wolves2 (sample "samples/wolves.wav"))

(definst slidinator [buf-id 0
                     depth {:default 50 :min 1 :max 1000}
                     freq  {:default 20 :min 1 :max 5000}
                     start {:default 0 :min 0.1 :max 1.0}
                     end   {:default 1 :min 0 :max 1.0}
                     amp   {:default 1 :min 0 :max 1.0}
                     gate 1
                     rate 1
                     trig 1
                     reset-pos 0]
  (let [samples (buf-frames buf-id)
        start   (+ depth (* start samples))
        end     (* end (- samples depth))
        dur     (/ (- end start) (sample-rate))
        pos     (phasor:ar trig rate start end reset-pos)]
    (* amp
       (env-gen (envelope [0 1 0] [0.01 dur] :linear 2) gate :action FREE)
       (env-gen (envelope [0 1 1 0] [0.01 (- dur 0.02) 0.01]))
       (buf-rd 2 buf-id (lin-lin (var-saw freq)
                                 -1 1
                                 (- pos depth) (+ pos depth))))))

(ui/inst-ui! slidinator)

(do
  (stop)
  (slidinator wolves
              :freq 500
              :depth 80))

(ploop :w {:instrument slidinator
           :buf-id wolves2
           :freq nil
           })

(overtone.studio.event/params-vec  {:instrument slidinator
                                    :buf-id wolves2
                                    :freq nil})

(require '[casa.squid.plasticine :as p])

(defn inst-ctls [inst]
  (for [{:keys [name min max value step]} (:params inst)
        :when (and min max)]
    (p/hslider {:min min
                :max max
                :step step
                :model value
                :on-change #(ctl inst name %)
                :height 60
                :format #(format "%s: %.2f" name (double %))})))

(def app (p/stack (inst-ctls slidinator) :margin 4 :gap 4))

(q/defsketch xxx
  :settings    #(q/smooth 2)
  :features    [:resizable :keep-on-top]
  :middleware  [p/middleware]
  ::p/root     #'app
  :size        [323 200]
  ::p/defaults {:text-size     25
                :frame-rate    30
                :stroke        [0 0 0]
                :fill          [0 0 0]
                :stroke-weight 15
                :background    [235 214 125]
                :rect-mode     :corner
                :stroke-cap    :round})

(stop)
(wolves)

(definst tonk [freq 200 amp 1 gate 1]
  (* amp
     (env-gen (asr 0 1 0.1) :gate gate :action FREE)
     (lpf
      (pluck (white-noise) :delaytime (/ 1 freq))
      (line 0 20000 0.3))))

(definst gendi [freq 400 amp 1 gate 1]
  (* amp
     (env-gen (asr) :gate gate :action FREE)
     (moog-ff (gendy3 :freq freq) (line 1000 20000 1))
     ))
[(into {} x) (into {}gendi)]
(def x gendi)
(pp-node-tree)
(defsynth reverb [in-bus 0
                  out-bus 0
                  roomsize {:default 3 :min 0.1 :max 20}
                  revtime {:default 2.0 :min 0.1 :max 5.0}
                  damping {:default 0.5 :min 0 :max 1}
                  inputbw 0.5
                  spread 15.0
                  drylevel 0
                  earlyreflevel 0.7
                  taillevel 0.5
                  maxroomsize 300.0]
  (out out-bus
       (g-verb (in in-bus)
               roomsize
               revtime
               damping
               inputbw
               spread
               drylevel
               earlyreflevel
               taillevel
               maxroomsize)))

(def aux-reverb-bus (audio-bus))
(def rev (reverb [:tail (foundation-safe-post-default-group)] aux-reverb-bus 0))

(ui/synth-ui! reverb rev)

(defsynth aux-send [bus 0 send-bus 0 amount 0]
  (let [sig (in bus)]
    (out send-bus (* sig amount))))

(def gendi-reverb
  (inst-fx! gendi aux-send :send-bus aux-reverb-bus))

(pp-node-tree)

(demo (out 0 (g-verb (in aux-reverb-bus) :drylevel 0)))

(clear-instruments)
(stop)
(ctl gendi-reverb :amount 1)
(into {} gendi)
(gendi)
inst-fx!
(ctl rev :roomsize 2)

(kill sig)
(pp-node-tree)
(defsynth sig []
  (out aux-reverb-bus (sin-osc)))
(sig2)
(kill sig2)

(stop)

(pp-node-tree)
(stop)
(ppause :h)
(ploop :h {:instrument (f/fsample :hihat-closed)
           :note [:- 0]
           :dur 1/2
           :amp (pwhite 0.5 0.8)
           })

(ppause :4)
(ploop :4 {:instrument (f/fsample :kick-fat)
           })

(ppause :t)
(ploop :t {:instrument gendi
           :degree [4 (repeat 5 1)
                    5 (repeat 5 1)]
           :octave 3
           :amp [(range 0 0.7 0.01) (pwhite 0.5 0.8)]
           ;; :amp [(reverse (range 0 0.7 0.01)) (repeat 0)]
           :dur [1/2 1/4 1/4]})

(ploop :f {:instrument tonk
           :degree (pchoose [[8 4 3 5 4]
                             [5 5 5 6 8]])
           :octave 3
           :amp 0.5
           :dur [1/2 1/4 1/2 3/4 6]})

(ploop :g {:instrument tonk
           :degree [:- :- 2 :- 3 :- :- 7]
           :dur [1/4]})
(stop)
(ploop :f {:instrument floop
           :note [[:c :- :- :- :- :- :- :-]
                  [:- :- :c :- :c :d :- :-]
                  [:c :- :- :- :- :- :- :-]
                  [:- :- :- :- :- :c :c :-]]
           :dur 1/4
           })
(stop)
(definst floop [freq 400
                amp 1
                gate 1
                ]
(* amp
   (env-gen (envelope [0 1 0] [0.01 0.3 0.05] )
            :gate gate
            :action FREE)
   (moog-ff (var-saw freq)
            (line 0 6000 0.4))))

(do
(tonk 100)
(s/bell))


((f/fsample :kick))
((f/fsample :hihat-click))
()
