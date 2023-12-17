(ns mad-sounds.sessions.aarschot-2023-12-15-ui
  (:require
   [clojure.java.io :as io]
   [quil.core :as q]))

(def ^:dynamic *drawing-stack* {})
(def ^:dynamic *drawing-component* nil)

(def quil-setter
  (memoize
   (fn [k]
     (resolve (symbol "quil.core" (name k))))))

(defn prop
  ([k]
   (get *drawing-stack* k))
  ([k not-found]
   (get *drawing-stack* k not-found)))

(defn set-prop! [k v]
  (if (vector? v)
    (apply (quil-setter k) v)
    ((quil-setter k) v)))

(defmacro with-props [props & body]
  `(let [old# *drawing-stack*
         new# ~props
         ks# (filter #(not= (get old# %) (get new# %)) (keys new#))]
     (doseq [k# ks#
             :let [v# (get new# k#)]]
       (set-prop! k# v#))
     (let [res# (binding [*drawing-stack* (merge old# new#)]
                  ~@body)]
       (doseq [k# ks#
               :when (contains? old# k#)
               :let [v# (get old# k#)]]
         (set-prop! k# v#))
       res#)))

(defn init-props! [props]
  (doseq [[k v] props]
    (set-prop! k v))
  (alter-var-root #'*drawing-stack* (constantly props)))

(defn polygon [& points]
  (doseq [[[x1 y1] [x2 y2]]
          (partition 2 1 (cons (last points) points))]
    (q/line x1 y1 x2 y2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro defdispatch [n args & default]
  (let [kw (keyword n)]
    `(defn ~n [~@args]
       (if-let [f# (get (meta ~(first args)) ~kw)]
         (f# ~@args)
         (do ~@default)))))

(defdispatch -draw [this x y w h])
(defdispatch -min-size [this] [0 0])
(defdispatch -max-size [this] [Long/MAX_VALUE Long/MAX_VALUE])
(defdispatch -pref-size [this] [100 100])
(defdispatch -key-pressed [this])
(defdispatch -key-released [this])
(defdispatch -key-typed [this])
;; (defdispatch -mouse-entered [this])
;; (defdispatch -mouse-exited [this])
;; (defdispatch -mouse-pressed [this])
;; (defdispatch -mouse-released [this])
;; (defdispatch -mouse-clicked [this])
;; (defdispatch -mouse-moved [this])
;; (defdispatch -mouse-dragged [this])
;; (defdispatch -mouse-wheel [this])

(defn delegate [c f & args]
  (let [cval (with-meta @c (meta c))]
    (if-let [props (:props cval)]
      (with-props props
        (apply f cval args))
      (apply f cval args))))

(defn find-root [c]
  (if-let [p (:parent (meta c))]
    (recur p)
    c))

(defn draw [c x y w h]
  (alter-meta! c assoc :parent *drawing-component*)
  (let [root (find-root c)]
    (when (and (:focusable? (meta c))
               (not (:focused (meta root))))
      (alter-meta! root assoc :focused c)))
  (binding [*drawing-component* c]
    (delegate c -draw x y w h))
  (swap! c assoc :bounds [x y w h]))

(defn min-size [c] (delegate c -min-size))
(defn max-size [c] (delegate c -max-size))
(defn pref-size [c] (delegate c -pref-size))

(defn c-text-draw [{:keys [text props]} x y w h]
  (let [tw (q/text-width text)
        th (prop :text-size 12)]
    (q/text text
            (max x (+ x (/ w 2) (- (/ tw 2))))
            (max y (+ y (/ h 2) (- (/ th 2))))
            w
            h)))

(defn c-text-min-size [{:keys [text]}]
  [(q/text-width text) (prop :text-size 12)])

(defn c-text-pref-size [{:keys [text]}]
  (let [text-size (prop :text-size 12)]
    [(+ (q/text-width text) text-size)
     (* 2 text-size)]))

(defn c-outline-draw [{:keys [child props]} x y w h]
  (let [sw (prop :stroke-weight)
        x (+ x (/ sw 2))
        y (+ y (/ sw 2))
        w (- w sw)
        h (- h sw)]
    (polygon [x y]
             [(+ x w) y]
             [(+ x w) (+ y h)]
             [x (+ y h)])
    (draw child
          (+ x (/ sw 2))
          (+ y (/ sw 2))
          (- w sw )
          (- h sw ))))

(defn c-outline-delegate-size [size-f]
  (fn [{:keys [child]}]
    (let [sw (prop :stroke-weight)
          [w h] (size-f child)]
      [(+ sw w) (+ sw h)])))

(defn container-draw [{:keys [children layout-f]} x y w h]
  (doseq [[child x y w h] (layout-f children x y w h)]
    (draw child x y w h)))

(defn layout-rows [children x y w h]
  (let [rh (/ h (count children))]
    (for [[idx child] (map vector (range) children)]
      [child x (+ y (* idx rh)) w rh])))

(defn layout-cols [children x y w h]
  (let [cw (/ w (count children))]
    (for [[idx child] (map vector (range) children)]
      [child (+ x (* idx cw)) y cw h])))

(defn layout-stack [children x y w h]
  (second
   (reduce (fn [[yy res] ch]
             (let [[pw ph] (pref-size ch)]
               (if (<= (+ yy ph) h)
                 [(+ yy ph)
                  (conj res [ch x yy (min pw w) ph])]
                 (reduced [nil res]))))
           [y []] children)))

(defn container-size [{:keys [children layout-f bounds]}]
  (if-let [[x y w h] bounds]
    (let [layout (layout-f children x y w h)
          x (apply min (map (fn [_ x _ _ _] x) layout))
          y (apply min (map (fn [_ _ y _ _] y) layout))]
      [(- (apply max (map (fn [_ x _ w _] (+ x w)) layout)) x)
       (- (apply max (map (fn [_ _ y _ h] (+ y h)) layout)) y)])
    (default-max-size)))

;;;;;;;;;;;;;;;
;; API
(defn text-c [text & {:as props}]
  (atom
   {:text text :props props}
   :meta
   {:-draw      #'c-text-draw
    :-min-size  #'c-text-min-size
    :-pref-size #'c-text-pref-size}))

(defn outline-c [child & {:as props}]
  (atom
   {:text child :props props}
   :meta
   {:-draw      #'c-outline-draw
    :-min-size  (c-outline-delegate-size min-size)
    :-pref-size (c-outline-delegate-size pref-size)}))

(defn container [children layout-f]
  (atom
   {:children children
    :layout-f layout-f}
   :meta
   {:-draw      #'container-draw
    :-min-size  #'container-size
    :-pref-size #'container-size
    :-max-size  #'container-size}))

(defn rows [children] (container children layout-rows))
(defn cols [children] (container children layout-cols))
(defn stack [children] (container children layout-stack))

(defn select-list-draw [{:keys [children index]} x y w h]
  (doseq [[idx [child x y w h]] (map list
                                     (range)
                                     (layout-stack children x y w h))]
    (when (= idx index)
      (with-props {:fill [190 190 250]
                   :stroke-weight 0}
        (q/rect x y w h)))
    (draw child x y w h)))

(defn mark-dirty! [c]
  (swap! (find-root c) assoc :dirty? true)
  c)

(defn select-list [children]
  (atom
   {:children   children
    :layout-f   stack
    :index      0}
   :meta
   {:focusable? true
    :-draw      #'select-list-draw
    :-min-size  #'container-size
    :-pref-size #'container-size
    :-max-size  #'container-size
    :-key-pressed (fn [this]
                    (case (q/key-as-keyword)
                      :down (swap! this update :index inc)
                      :up  (swap! this update :index dec)
                      nil)
                    (mark-dirty! this))}))

;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn draw-root [c]
  (when (or (:dirty? @c)
            (not= [0 0 (q/width) (q/height)] (:bounds @c)))
    (swap! c assoc :dirty? false)
    (set-prop! :background (prop :background))
    (draw c 0 0 (q/width) (q/height))))

;;;;;;;;;;;;

;; - location
;; - bounds

(def defaults
  {:text-size 110
   :frame-rate 30
   :stroke [0 0 0]
   :fill [26 191 72]
   :stroke-weight 15
   :background [235 214 125]
   :rect-mode :corner
   :stroke-cap :round})

(defn setup []
  (init-props! defaults))

(def app
  (mark-dirty!
   (select-list
    [(text-c "Hello" :fill [54 121 44] :text-size 50)
     (text-c "Hello" :fill [54 121 44] :text-size 50)
     (text-c "Hello" :fill [54 121 44] :text-size 50)
     ])))

(defn draw-fn []
  (with-props defaults
    (draw-root app)))

(defn key-pressed-fn [root] (-key-pressed (:focused (meta root))))
(defn key-released-fn [root] (-key-released (:focused (meta root))))
(defn key-typed-fn [root] (-key-typed (:focused (meta root))))

(q/defsketch example
  :title "UI test"
  :settings #(q/smooth 2) ;; Turn on anti-aliasing
  :features [:resizable :keep-on-top]
  :setup #'setup
  :draw #'draw-fn
  :key-pressed (partial #'key-pressed-fn app)
  :key-released (partial #'key-released-fn app)
  :key-typed (partial #'key-typed-fn app)
  :size [323 200])

(alter-var-root #'quil.applet/*applet* (constantly example))
