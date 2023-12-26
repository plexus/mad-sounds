(ns mad-sounds.sessions.aarschot-2023-12-26-patchbay
  (:require
   [casa.squid.jack :as jack]
   [casa.squid.plasticine :as p]
   [overtone.at-at :as at]
   [overtone.live :refer :all]
   [overtone.studio.midi :as midi]
   [quil.core :as q]
   [vibeflow.util :as util]))

(jack/connect (util/overtone-conns))

(do
  (defn draw-in-ports [{:keys [offset-x offset-y spacing]} x y w h]
    (doseq [[port idx] (map list (jack/ports @jack/default-client #{:audio :in}) (range))
            :let [xx (+ x offset-x (* idx spacing))
                  yy (+ y offset-y)]]
      (q/push-matrix)
      (q/translate xx yy)
      (q/rotate -1)
      (q/translate (- xx) (- yy))
      (q/text port xx yy)
      (q/line (+ 3 xx)
              (+ (/ spacing 4) yy)
              (+ xx w)
              (+ (/ spacing 4) yy))
      (q/pop-matrix)
      (q/line (+ (/ spacing 4) xx)
              (+ 3 yy)
              (+ (/ spacing 4) xx)
              (+ (/ spacing 4) yy (* spacing (count (jack/ports @jack/default-client #{:audio :out})))))
      )
    #_(jack/ports @jack/default-client #{:audio :in})
    #_  (jack/ports @jack/default-client #{:audio :out})
    )

  (defn draw-out-ports [{:keys [offset-x offset-y spacing]} x y w h]
    (doseq [[port idx] (map list (jack/ports @jack/default-client #{:audio :out}) (next (range)))
            :let [x (+ x spacing)
                  y (+ y offset-y (* idx spacing))]]
      (q/text port x y)
      (q/line x (+ (/ spacing 4) y) (+ x w (- (* 2 spacing))) (+ (/ spacing 4) y))))

  (jack/connections)

  (defn draw-connections [{:keys [offset-x offset-y spacing]} x y w h]
    (let [conn? (set (jack/connections))]
      (doseq [[out-port out-idx] (map list (jack/ports @jack/default-client #{:audio :out}) (range))
              [in-port in-idx] (map list (jack/ports @jack/default-client #{:audio :in}) (next (range)))
              :let [xx (+ x offset-x (* in-idx spacing) (- (* 5/4 spacing)))
                    yy (+ y offset-y (* out-idx spacing) (* 3/4 spacing))
                    conn? (conn? [out-port in-port])]]
        (when conn?
          (p/with-props {:stroke-weight 3
                         :stroke [78 150 207]}
            (q/line (+ x (q/text-width out-port) (* 5/4 spacing))
                    yy
                    xx
                    yy)
            (q/line xx
                    (+ y offset-y (* 1/4 spacing))
                    xx
                    yy)))
        (p/with-props {:fill (if conn?
                               [94 219 110]
                               [255]
                               )}
          (q/ellipse xx yy 10 10)))))

  (defn draw-patchbay [this x y w h]
    (draw-in-ports this x y w h)
    (draw-out-ports this x y w h)
    (draw-connections this x y w h))

  (defn patchbay-mouse-clicked [this e]
    (let [{:keys [offset-x offset-y spacing bounds]} @this
          [x y w h] bounds
          mx (:x e)
          my (:y e)
          conn? (set (jack/connections))]
      (doseq [[out-port out-idx] (map list (jack/ports @jack/default-client #{:audio :out}) (range))
              [in-port in-idx] (map list (jack/ports @jack/default-client #{:audio :in}) (next (range)))
              :let [xx (+ x offset-x (* in-idx spacing) (- (* 5/4 spacing)))
                    yy (+ y offset-y (* out-idx spacing) (* 3/4 spacing))
                    conn? (conn? [out-port in-port])]]
        (when (and (< (- xx (* 1/2 spacing)) mx (+ xx (* 1/2 spacing)))
                   (< (- yy (* 1/2 spacing)) my (+ yy (* 1/2 spacing))))
          (try
            (if (some #{[out-port in-port]} (jack/connections))
              (jack/disconnect out-port in-port)
              (jack/connect out-port in-port))
            (catch Exception e))
          (p/mark-dirty! this)))))

  (def app (atom
            {:offset-x 500
             :offset-y 400
             :spacing 35
             :props {:stroke-weight 1}}
            :meta {:-draw #'draw-patchbay
                   :-mouse-clicked #'patchbay-mouse-clicked})  ))

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
