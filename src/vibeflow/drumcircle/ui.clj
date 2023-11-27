(ns vibeflow.drumcircle.ui
  (:require
   [quil.core :as q]
   [vibeflow.drumcircle :as drumcircle]))

(def colors {:green  [0x3A 0x60 0x29]
             :yellow [0xF6 0xCE 0x1F]
             :red    [0xA6 0x3A 0x2B] #_[0xE1 0x24 0x0B]
             :ochre  [0xD1 0x8D 0x2C]
             :brown  [0x84 0x4F 0x21]
             :blue   [0x6E 0xAB 0xB7]
             :light1 [0xEF 0xEF 0xE6]
             :light2 [0xDE 0xD8 0xC2]
             :light3 [0xA1 0xA5 0x86]})

(def background [0xE3 0xD2 0x85])

(def canvas-size 900)

(def rings [[:kick :red]
            [:snare :green]
            [:hi-hat :blue]
            [:hi-hat-open :yellow]])

(def TAU (* Math/PI 2))

(defn stroke-color [k]
  (let [[r g b] (get colors k)]
    (q/stroke r g b)))

(defn setup []
  (q/no-fill)
  (q/stroke-cap :square)
  (q/no-loop))

(defn arc-segment [{:keys [rx ry
                           ring
                           segment
                           arc-height
                           arc-width
                           arc-margin
                           arc-offset
                           stroke
                           ]}]
  (let [ring-size (- (* ring arc-height) stroke)]
    (q/arc rx ry
           ring-size ring-size
           (+ arc-offset (* segment arc-width) arc-margin)
           (+ arc-offset (* (inc segment) arc-width) (- arc-margin)))))

(def parameters
  (let [rsize (* canvas-size 2/3)
        rx (/ canvas-size 2)
        ry (/ canvas-size 2)
        ring-count (count rings)
        margin 3
        segments 16
        arc-height (/ rsize ring-count)
        arc-width (/ TAU segments)
        arc-offset (- 0 (/ TAU 4) (/ TAU segments 2))
        stroke (- (/ arc-height 2) margin)
        circumference (* Math/PI rsize)
        arc-margin (* TAU (/ margin circumference))]
    {:ry ry
     :margin margin
     :rx rx
     :stroke stroke
     :arc-height arc-height
     :arc-width arc-width
     :circumference circumference
     :arc-margin arc-margin
     :rsize rsize
     :segments segments
     :ring-count ring-count
     :arc-offset arc-offset}))

(defn current-segment [{:keys [beat bar tick ticks-per-beat beats-per-bar] :as state}]
  (let [ticks-in-bar (* ticks-per-beat beats-per-bar)
        current-tick-in-bar (+ tick (* ticks-per-beat beat))]
    (if (zero? ticks-in-bar)
      0
      (mod
       (+ (Math/floor (* 16 (/ current-tick-in-bar ticks-in-bar))) 12)
       16))))

(defn draw [{:keys [pattern] :as state}]
  (apply q/background background)
  (let [{:keys [rsize ry rx ring-count
                margin segments
                arc-height arc-width  arc-offset
                stroke circumference arc-margin]}
        parameters]
    (q/stroke-weight stroke)
    (doseq [[[inst color] ring] (map vector rings (range))
            segment (range segments)]
      (if (some #{[(/ segment segments) inst]} pattern)
        (stroke-color color)
        (stroke-color (cond
                        (= 0 (mod segment 4)) :light3
                        (even? segment) :light2
                        :else :light1)))
      (arc-segment {:rx rx
                    :ry ry
                    :ring (- ring-count ring)
                    :segment segment
                    :arc-height arc-height
                    :arc-width arc-width
                    :arc-margin arc-margin
                    :arc-offset arc-offset
                    :stroke stroke}))
    (stroke-color :blue)
    (arc-segment {:rx rx
                  :ry ry
                  :ring (inc ring-count)
                  :segment (current-segment state)
                  :arc-height arc-height
                  :arc-width arc-width
                  :arc-margin arc-margin
                  :arc-offset arc-offset
                  :stroke stroke})))

(defn polar [x y]
  [(Math/sqrt (+ (* x x) (* y y)))
   (Math/atan2 x y)])

(defn tau-wrap [x]
  (if (< TAU x)
    (recur (- x TAU))
    x))

(defn on-mouse-clicked []
  (let [{:keys [rsize ry rx ring-count
                margin segments
                arc-height arc-width  arc-offset
                stroke circumference arc-margin]}
        parameters
        x (- (q/mouse-x) rx)
        y (- (q/mouse-y) ry)
        [r theta] (polar x y)
        [inst _] (->> (* (/ (* r 2) rsize) ring-count)
                      Math/ceil
                      long
                      (- ring-count)
                      (get rings))
        segment (long (Math/floor (* segments (/ (tau-wrap (+ (- TAU (+ Math/PI theta))
                                                              (/ TAU segments 2)))
                                                 TAU))))
        note [(/ segment segments) inst]]
    (when inst
      (swap! drumcircle/state update :pattern
             #(if (contains? % note)
                (disj % note)
                (conj % note))))))

(defn start-ui []
  (let [watch-key (keyword (str *ns*) (str (gensym "draw")))
        applet (q/sketch :title "Drumcircle"
                         :settings #(q/smooth 4)
                         :setup (fn []
                                  (setup))
                         :draw (fn []
                                 (draw @drumcircle/state))
                         :size [canvas-size canvas-size]
                         :features [:resizable]
                         :on-close #(remove-watch drumcircle/state watch-key)
                         :key-typed (fn [] (when (= :space (q/key-as-keyword))
                                             (drumcircle/play-pause!)))
                         :mouse-clicked #'on-mouse-clicked)]
    (add-watch drumcircle/state
               watch-key
               (fn [_ _ _ _]
                 (quil.applet/with-applet applet
                   (q/redraw))))
    applet))

(defn drumcircle []
  (drumcircle/start-sequencer)
  (start-ui))

(comment
  (drumcircle)
  (swap! pattern update :tick (fn [t] (mod (inc t) 16)))
  (swap! pattern update :pattern conj [0 1]))
