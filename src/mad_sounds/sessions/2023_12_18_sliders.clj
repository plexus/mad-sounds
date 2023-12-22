(ns mad-sounds.sessions.2023-12-18-sliders
  (:require
   [casa.squid.plasticine :as p]
   [clojure.java.io :as io]
   [quil.core :as q]))

(def app
  (p/stack
   [(p/hslider {:max 100
                :value 33
                :height 60})
    (p/hslider {:max 100
                :value 69
                :height 60
                :bar{:fill [132 124 212]
                     :stroke 255
                     :stroke-weight 2}})]
   :margin 4
   :gap 4))

(q/defsketch example
  :title       "Plasticine Sliders"
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
