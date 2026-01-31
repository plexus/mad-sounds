(ns mad-sounds.sessions.aarschot-2026-01-30-postbirthdaychill
  (:require
   [quil.core :as q]
   [casa.squid.jack :as jack]
   [casa.squid.plasticine :as p]
   [clojure.java.io :as io]
   [overtone.live :refer :all]
   [vibeflow.jackutil :as jackutil]
   [vibeflow.util :as util :refer [ctl!]]))

(defonce jack-client (jack/client "vibeflow"))
(jackutil/monitor jack-client)

(def app
  (p/grid
   :cols [{:min 50 :max 50} {:min 50 :max 50}]
   :rows 2
   :children
   [(p/hslider {:max 100
                :value 33
                :height 60})
    (p/hslider {:max 100
                :value 69
                :height 60
                :bar{:fill [132 124 212]
                     :stroke 255
                     :stroke-weight 2}})
    (p/hslider {:max 100
                :value 33
                :height 60})
    (p/hslider {:max 100
                :value 69
                :height 60
                :bar{:fill [132 124 212]
                     :stroke 255
                     :stroke-weight 2}})])
  #_(p/stack
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

(q/defsketch example3
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
