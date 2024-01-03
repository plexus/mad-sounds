(ns vibeflow.controller-ui
  (:require
   [casa.squid.plasticine :as p]
   [quil.core :as q]
   [overtone.core :as o]))

(def exclude?
  #{"gate" "note" "freq"})

(defn sliders-for [synth]
  (for [{:keys [name min max value step]}
        (:params synth)
        :when (not (exclude? name))]
    (p/hslider {:min min
                :max max
                :step step
                :model value
                :on-change #(o/ctl synth name %)
                :height 60
                :format #(if (int? %)
                           (format "%s: %d" name %)
                           (format "%s: %.2f" name (double %)))})))

(def app
  (p/stack [] :margin 4 :gap 4))

(defn ctl-synth! [synth]
  (let [synth (if (var? synth) @synth synth)]
    (swap! app assoc :children (sliders-for synth))))

(defn show! []
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
                  :stroke-cap    :round}))
