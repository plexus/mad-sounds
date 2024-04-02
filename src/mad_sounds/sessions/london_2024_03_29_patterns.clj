(ns mad-sounds.sessions.london-2024-03-29-patterns
  (:require
   [mad-sounds.euclid :refer :all]
   [overtone.core :refer :all]
   [overtone.studio.transport :as transport]))

;; pw-jack scsynth -u 55555
(connect-server 55555)
(overtone.sc.machinery.server.connection/connect-jack-ports)

(definst mucklewain [freq 440 gate 1]
  (splay
   (* (env-gen (adsr) :gate gate :action FREE)
      (rlpf (square (freq-spread freq 4))
            (* 5 freq)
            0.1))
   :spread (env-gen (envelope [1 0 1] [0.05 0.05]))
   :level-comp? false))

(definst gloria [freq 265
                 gate 1.0
                 amp 1]
  (splay
   (* (env-gen (adsr :attack 0.009 :decay 0.2 :sustain 0.5) :gate gate :action FREE)
      amp
      (+ (* 0.5 (sin-osc freq))
         (pluck (brown-noise)
                :maxdelaytime 1
                :delaytime (/ 1 (freq-spread freq 3 0.5)))))
   :level-comp? false))
(stop)
(definst snare [rel 0.3
                amp 1]
  (pan2
   (* (env-gen (perc :release rel) :action FREE)
      amp
      (pluck
       (white-noise)
       :delaytime (env-gen
                   (envelope [35/1000 25/1000 10/1000 20/1000]
                             (repeat 3 (/ rel 3))))
       :coef (env-gen (envelope [-0.1 -0.2] [rel]))))))

(definst tick [amp 1]
  (pan2
   (* (env-gen (perc :release 0.08) :action FREE)
      amp
      (hpf
       (pluck
        (white-noise)
        :delaytime 0.018
        :coef 0.1)
       2000))))

(metro-bpm transport/*clock* 100)

(pplay :bass
       {:degree     [5 1 3 4 1]
        :type       :chord
        :chord      [:major :major :minor :major :major]
        :amp        (pwhite 0.3 0.5)
        :dur        [4]
        :instrument mucklewain
        :octave     2}
       {:quant 20})

(defn parp [notes intervals]
  (for [n notes
        i intervals]
    (+ n i)))

(parp [0 1 2] [0 3 5])

(pplay :gloria
       {:degree (cycle (concat (parp [5 1 3 4 1]
                                     [0 -2 -2 4 6 5])
                               #_               (parp [5 1 3 4 1]
                                                      [0 3 -2 5])))
        :instrument gloria
        :amp (pwhite 0.3 0.5)
        :dur [1 1 1/2 1/2 1/3 2/3]
        :octave 5}
       {:quant 20})

(premove :gloria)

(pplay :tick
       {:instrument tick
        :note [0 0 0 :x 0 0 :x 0]
        :amp (pwhite 0.2 0.4)})

(pplay :snare
       {:instrument snare
        :dur (cycle [4])
        :amp (pwhite 0.3 0.6)}
       {:offset 0.5
        :quant 4})

(pplay :g {:instrument gloria
           :degree (take 20 (pwhite 0 7))
           :amp (map #(/ 0.2 %) (range 20))
           :dur [0.5]})

(reset! overtone.sc.machinery.server.comms/osc-debug* false)
(event-debug-off)

(stop)
(pclear)
(ppause :gloria)
(presume :gloria)


(demo (* (env-gen (perc :release 0.1))
         (rlpf
          (pluck
           (white-noise))
          6000)))

(demo
  (let [freq (midicps 50)]
    (pan2
     (* (env-gen (perc))
        (sin-osc freq
                 :phase (sin-osc (* 5/4 freq)
                                 #_#_:phase (sin-osc (* 2/5 freq))))))))

(premove :tick)
(premove :snare)
(premove :bass)
(premove :gloria)
