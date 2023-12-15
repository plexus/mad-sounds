(ns mad-sounds.sessions.aarschot-2023-12-15-ui
  (:require
   [clojure.java.io :as io]
   [quil.core :as q]))

(def ^:dynamic *drawing-stack* {})

(def setter
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
    (apply (setter k) v)
    ((setter k) v)))

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

(defprotocol Component
  (draw [this x y w h])
  (min-size [this])
  (max-size [this])
  (pref-size [this]))

(defrecord TextC [text props]
  Component
  (draw [this x y w h]
    (with-props (assoc props :rect-mode :corner)
      (let [tw (q/text-width text)
            th (prop :text-size 12)]
        (q/text text
                (max x (+ x (/ w 2) (- (/ tw 2))))
                (max y (+ y (/ h 2) (- (/ th 2))))
                w
                h))))
  (min-size [_]
    (with-props props
      [(q/text-width text) (prop :text-size 12)]))
  (pref-size [_]
    (with-props props
      (let [text-size (prop :text-size 12)]
        [(+ (q/text-width text) text-size)
         (* 2 text-size)])))
  (max-size [_]
    [Long/MAX_VALUE Long/MAX_VALUE]))

(defrecord OutlineC [child props]
  Component
  (draw [this x y w h]
    (with-props props
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
              (- h sw )))))
  (min-size [_]
    (with-props props
      (let [sw (prop :stroke-weight)
            [w h] (min-size child)]
        [(+ sw w) (+ sw h)])))
  (pref-size [_]
    (with-props props
      (let [sw (prop :stroke-weight)
            [w h] (pref-size child)]
        [(+ sw w) (+ sw h)])))
  (max-size [_]
    [Long/MAX_VALUE Long/MAX_VALUE]))

(defrecord RowsC [children]
  Component
  (draw [this x y w h]
    (let [rh (/ h (count children))]
      (doseq [[idx child] (map vector (range) children)]
        (draw child x (+ y (* idx rh)) w rh)))))

(defrecord ColsC [children]
  Component
  (draw [this x y w h]
    (let [cw (/ w (count children))]
      (doseq [[idx child] (map vector (range) children)]
        (draw child (+ x (* idx cw)) y cw h)))))

(defn draw-root [c]
  (set-prop! :background (prop :background))
  (draw c 0 0 (q/width) (q/height)))

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
   :stroke-cap :square})

(defn setup []
  (init-props! defaults))

(def root
  (->RowsC
   [(->OutlineC
     (->TextC "Hello" {:fill [54 121 44]
                       :text-size 50})
     {:stroke [191 26 105]})
    (->OutlineC
     (->TextC "World" {:fill [154 0 144]
                       :text-size 80})
     {:stroke [33 26 191]})
    (->ColsC
     [(->OutlineC
       (->TextC "World" {:fill [154 0 144]
                         :text-size 80})
       {:stroke [33 26 191]
        :stroke-weight 269})
      (->OutlineC
       (->TextC "World" {:fill [154 0 144]
                         :text-size 80})
       {})])]))

(defn draw-fn []
  (with-props defaults
    (draw-root @#'root)))

(q/defsketch example
  :title "UI test"
  :settings #(q/smooth 2) ;; Turn on anti-aliasing
  :features [:resizable :keep-on-top]
  :setup #'setup
  :draw #'draw-fn
  :size [323 200])
