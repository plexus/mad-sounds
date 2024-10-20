(ns mad-sounds.sessions.conj-demo-2024-10-20
  (:require
   [overtone.live :refer :all]
   [overtone.studio.transport :as transport]
   [vibeflow.freesound :as f]
   [vibeflow.studio :as ui]
   [vibeflow.synths :as s]))

(demo (sin-osc))
(stop)
(transport/*clock* :bpm 165)

(definst gendi [freq 400
                mix {:default 0.5 :min 0 :max 1}
                min-freq {:default 0 :min 0 :max 2000}
                max-freq {:default 4000 :min 0 :max 10000}
                slope-dur {:default 1 :min 0.1 :max 2}
                detune {:default 1 :min 0.8 :max 3}
                amp 1
                gate 1]
  (* amp
     (env-gen (adsr 0.01 0.01 0.6) :gate gate :action FREE)
     (lpf
      (+ #_(* 0.1 (env-gen (perc)) (white-noise))
         (* mix (sin-osc freq (var-saw freq)))
         #_(* (- 1 mix) (sin-osc freq (var-saw freq :width 0.1)))
         #_         (* mix (sin-osc freq (var-saw (* detune (lin-exp (var-saw freq) -1 1 1 1.1) freq)
                                                  )))
         (* mix (sin-osc (* 2 freq) (var-saw (* detune (lin-exp (var-saw freq) -1 1 1 1.1) freq)
                                             )))
         (* (- 1 mix) (pluck (white-noise) :delaytime (/ 1 freq)))
         #_(* (- 1 mix) (gendy3 :freq freq)))
      (* max-freq (line 1 0.1 0.5)))))

(ui/inst-ui! gendi)

(definst tonk [freq 200 amp 1 gate 1]
  (* amp
     (env-gen (asr 0 1 0.1) :gate gate :action FREE)
     (lpf
      (pluck (white-noise) :delaytime (/ 1 freq))
      (line 0 5000 0.3))))

(ppause :h)
(ploop :h {:instrument #_(f/fsample :hihat-closed)
           [(f/fsample :hihat-closed)
            (f/fsample :hihat-open2)
            (f/fsample :hihat-closed)
            (f/fsample :zg-hat)]
           :note [0 :-]
           :dur 1/2
           :amp (pwhite 0.5 0.8)
           })

(ppause :4)
(ppause :n)
(do
  (ploop :4 {:instrument (f/fsample :kick-fat)
             :note  [[0 :- :- :-]
                     [:-  0 :- :-]]
             :dur 1/2
             })

  (ploop :n {:instrument (f/fsample :snare-fat)
             :note[[:- :- 0 :-]]
             :dur 1/2
             }))
(stop)
(ppause :t)
(ploop :t {:instrument gendi
           ;; :degree [1 (repeat 5 :-)]
           :degree [4 (repeat 5 1)
                    5 (repeat 5 1)]
           :octave 3
           :amp 1 #_[(range 0 0.7 0.01) (pwhite 0.5 0.8)]
           ;; :amp [(reverse (range 0 0.7 0.01)) (repeat 0)]
           ;; :mix [(range 0.7 1 0.03)
           ;;       (reverse (range 0.8 1 0.05))]
           :dur [1/2 1/4 1/4]})

(ui/inst-ui! gendi)

(ppause :t)
(ploop :t {:instrument tonk
           :degree (pchoose [[0 0 0 0 5]])
           :octave 4
           :amp 0.5
           :mode :minor
           :dur [1/2 1/4 1/2 3/4 6]})

(ppause :b)
(ploop :b {:type [:note :note :chord :rest]
           :instrument s/bell
           ;; :degree [0 (repeat 7 :-)]
           ;; :degree [0 (repeat 5 :-) 7 :-]
           :degree (map #(do [% :- :- :-]) [0 2 6 5 3 4 5 7])
           ;; :degree (map #(repeat 4 %) [0 2 6 5 3 4 5 7])
           :dur 1/4
           :mode :minor
           :octave 4
           :amp (pwhite 0.7 1)
           }
       {:quant 8})

(ui/inst-ui! s/bell)

(ppause :w)
(ploop :w {;;:type [:note :note :chord :rest]
           ;; :type [:chord]
           :instrument s/wobblepad
           :degree [0 0 1 0]
           :mode :minor
           :octave 5
           :dur 4}
       {:quant 16})
(ui/inst-ui! s/wobblepad)

(stop)

(volume 1.5)
