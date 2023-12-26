(ns mad-sounds.sessions.aarschot-2023-12-23-vakantie-dag-1
  (:require
   [casa.squid.jack :as jack]
   [casa.squid.plasticine :as p]
   [overtone.at-at :as at]
   [overtone.live :refer :all]
   [overtone.studio.midi :as midi]
   [quil.core :as q]
   [vibeflow.util :as util]))

(jack/connect (util/overtone-conns))

(definst bell1 [note {:default 60 :min 0 :max 120 :step 1}
                mod-interval {:default 12 :min -12 :max 24 :step 1}
                attack {:default 0.001 :min 0 :max 1 :step 0.01}
                decay  {:default 0.2 :min 0 :max 2 :step 0.01}
                sustain  {:default 1 :min 0 :max 2 :step 0.01}
                release {:default 0.5 :min 0 :max 2 :step 0.01}
                depth {:default 1 :min 1 :max 10 :step 0.1}
                gate {:default 1 :min 0 :max 1 :step 1}
                detune {:default 0 :max 1}
                ]
  (let [freq    (midicps note)
        modfreq (midicps (+ note mod-interval))
        mod     (sin-osc :freq (* (+ 1 (* 1/50
                                          (env-gen
                                           (adsr attack decay 0 0)
                                           )))
                                  modfreq))]
    (* (env-gen (adsr attack decay sustain release)
                :gate gate
                :action FREE)

       (mix [(* 0.1 (sin-osc :freq (/ freq 2)))
             (sin-osc :freq freq :phase (* mod depth))
             (* 0.3 (sin-osc :freq (* 5.03 freq)))
             (* 0.4 (sin-osc :freq (* 6.13 freq)))]))))

(def stop? (volatile! false))

(vreset! stop? true)
(vreset! stop? false)
(stop)
(future
  (while (not @stop?)
    (bell1 :note 75)
    (Thread/sleep 300)
    (ctl bell1 :gate 0)
    (Thread/sleep 500)
    ))

(def sliders
  (for [{:keys [name min max value step]}
        (:params bell1)]
    (p/hslider {:min min
                :max max
                :step step
                :model value
                :on-change #(ctl bell1 name %)
                :height 60
                :bar
                :format #(str name ": " %)})))

(swap! app assoc :children into sliders)

(def app
  (p/stack sliders :margin 4 :gap 4))

(q/defsketch controllers
  :title       ""
  :settings    #(q/smooth 2) ;; Turn on anti-aliasing
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
