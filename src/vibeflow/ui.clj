(ns vibeflow.ui
  (:require
   [overtone.live :refer :all]
   [quil.core :as q]
   [casa.squid.plasticine :as p]))

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

(defn synth-ctls [synthdef synth]
  (for [{:keys [name min max value step]} (:params synthdef)
        :when (and min max)]
    (p/hslider {:min min
                :max max
                :step step
                :model value
                :on-change #(ctl synth name %)
                :height 60
                :format #(format "%s: %.2f" name (double %))})))

(defn ui! [title ctls cnt]
  (q/sketch
   :title title
   :settings    #(q/smooth 2)
   :features    [:resizable :keep-on-top]
   :middleware  [p/middleware]
   ::p/root     (atom ctls)
   :size        [323 (* 68 cnt)]
   ::p/defaults {:text-size     25
                 :frame-rate    30
                 :stroke        [0 0 0]
                 :fill          [0 0 0]
                 :stroke-weight 15
                 :background    [235 214 125]
                 :rect-mode     :corner
                 :stroke-cap    :round}))

(defn inst-ui! [inst]
  (let [ctls (inst-ctls inst)]
    (ui! (:name inst) (p/stack ctls :margin 4 :gap 4) (count ctls))))

(defn synth-ui! [synthdef synth]
  (let [ctls (synth-ctls synthdef synth)]
    (ui! (:name synthdef) (p/stack ctls :margin 4 :gap 4) (count ctls))))
