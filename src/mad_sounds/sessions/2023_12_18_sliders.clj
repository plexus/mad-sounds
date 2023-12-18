(ns mad-sounds.sessions.2023-12-18-sliders
  (:require
   [casa.squid.plasticine :as p]
   [clojure.java.io :as io]
   [quil.core :as q]))

(def defaults
  {:text-size 25
   :frame-rate 30
   :stroke [0 0 0]
   :fill [0 0 0]
   :stroke-weight 15
   :background [235 214 125]
   :rect-mode :corner
   :stroke-cap :round})

(def app
  (p/mark-dirty!
   (p/stack
    [(p/hslider {:max 100
                 :value 33
                 :height 60
                 :color [73 175 157]})
     (p/hslider {:max 100
                 :value 69
                 :height 60
                 :color [132 124 212]})])))

(q/defsketch example
  :title "UI test"
  :settings #(q/smooth 2) ;; Turn on anti-aliasing
  :features [:resizable :keep-on-top]
  :middleware [p/middleware]
  :ui/root app
  :ui/defaults defaults
  :size [323 200])
