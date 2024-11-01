(ns mad-sounds.sessions.conj-demo-2024-10-23
  (:require
   [overtone.live :refer :all]
   [vibeflow.freesound :as f]
   [vibeflow.ui :as ui]
   [vibeflow.synths :as s]))

(demo (sin-osc))

#_(pplay :b {:instrument s/churchbell
             :dur [6 6 6]})

(do
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
        (+
         #_(* (- 1 mix) (sin-osc freq (var-saw freq :width 0.1)))
         #_(* (- 1 mix) (gendy3 :freq freq))

         (* mix (+ (var-saw freq (var-saw freq))
                   (var-saw (* 3 freq)
                            (var-saw (* detune (lin-exp (var-saw freq) -1 1 1 1.1) freq)))))
         (* (- 1 mix) (pluck (white-noise) :delaytime (/ 1 freq)))
         )
        (* max-freq (line 1 0.1 0.5)))))

  (defn hat [& args]
    (ploop :hat (apply assoc {:instrument #_(f/fsample :hihat-closed)
                              [(f/fsample :hihat-closed)
                               (f/fsample :hihat-open2)
                               (f/fsample :hihat-closed)
                               (f/fsample :zg-hat)]
                              :note [0 :-]
                              :dur 1/2
                              :amp (pwhite 0.5 0.8)}
                       args)))

  (*clock* :bpm 160))

;;;;;;;;;;;;;;;;;;
(stop)

(ppause :w)

(ploop :w {:type [:note :note :chord :chord]
           :instrument s/wobblepad
           :degree [0 0 5 4]
           :mtranspose 0 #_4
           :mode :minor
           :octave 5
           :dur 4}
       {:quant 16
        :align :wait})

(ui/inst-ui! s/wobblepad)

(hat :note [0 :-])
(stop)
(ppause :gd)
(ploop :gd {:instrument gendi
            :degree [5 :- :- 4 1 :- 1 :-]
            :mode :minor
            :octave 3
            :mix 0.5
            :amp 0.7
            :dur 1/2}
       {:align :wait
        :quant 16})

(ploop :gd {:instrument gendi
            :degree [5 1 1 4 1 3 1 2]
            :mode :minor
            :octave 3
            :amp 0.7
            :dur 1/2}
       {:align :quant})

(ui/inst-ui! gendi)

(ploop :gd {:instrument gendi
            :degree [4 (repeat 5 1)
                     5 (repeat 5 1)]
            :octave 3
            :mix 0.5
            :amp 1 #_[(range 0 0.7 0.01) (pwhite 0.5 0.8)]
            ;; :amp [(reverse (range 0 0.7 0.01)) (repeat 0)]
            ;; :mix [(range 0.7 1 0.03)
            ;;       (reverse (range 0.8 1 0.05))]
            :dur [1/2 1/4 1/4]}
       {:align :quant})

(hat :note [0 :-])

(ppause :w)
(presume :w)
(ppause :kck)
(ppause :snr)

(hat :note 0)
(do
  (pplay :kck
         [{:instrument (f/fsample :revride)
           :start-pos 205000
           :dur 4}
          (repeat
           (pbind
            {:instrument (repeat (f/fsample :kick-fat))
             :note  [[0 :- :- :-]
                     [:- 0 :- :-]]
             :dur 1/2}
            ))]
         {:align :wait})

  (pplay :snr [{:type :rest
                :dur 4}
               (repeat
                (pbind
                 {:instrument (f/fsample :snare-fat)
                  :note [[:- :- 0 :-]]
                  :dur 1/2}))]
         {:align :wait}))

(stop)
(hat :note [0 :-])
(ppause :gd)
(ppause :dd)
(pplay :dd
       {:instrument [s/dickdale]
        :degree 0
        :octave 6
        :mode :minor
        :dur [4 0]}
       {:align :wait})
(stop)

(ppause :mar)
(ploop :mar
       {:instrument #_s/tonk s/marimba
        :degree [[3 1 :-] [:- 3 4 -1 0] [3 1 :-] [:- :- -1 0]]
        ;; :degree [:- :- -1 0]
        :mode :minor
        :mtranspose 0 #_[0 4]
        :dur [[1/2 1/2 3]
              [3/2 1 1/2 1/2 1/2]
              [1/2 1/2 3]
              [1 1 1 1]]}
       {:align :wait
        :quant 16
        })

(ui/inst-ui! s/marimba)

(stop)


(presume :w)
(presume :gd)

(ppause :w)
(ppause :gd)
(ppause :kck)
(ppause :snr)
(ppause :hat)
(ppause :mar)
(stop)

(s/marimba)
(ploop :m {:instrument s/marimba
           :degree [0 1 2]}
       )
