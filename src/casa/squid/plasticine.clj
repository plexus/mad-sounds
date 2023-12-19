(ns casa.squid.plasticine
  "Moldable clay UI lib for Quil"
  (:require
   [clojure.java.io :as io]
   [quil.core :as q]))

;; - A component is an atom with metadata
;; - Functions in the metadata act as methods
;; - The atom itself is its state/model
;; - The top level component is the root and does some extra book-keeping
;;   - :dirty? on the root will cause the whole UI to redraw (as does resizing the window)
;;   - :focused is the element that will receive keyboard events

(def ^:dynamic *drawing-stack* {})
(def ^:dynamic *drawing-component* nil)
(def ^:dynamic *root* nil)

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

(defn find-root [c]
  (if-let [p (:parent (meta c))]
    (recur p)
    c))

(defn mark-dirty! [c]
  (alter-meta! (find-root c) assoc :dirty? true)
  c)

(defn dispatch [c method & args]
  (when-let [f (get (meta c) method)]
    (apply f c args)))

(defn dispatch! [c method & args]
  (if-let [f (get (meta c) method)]
    (apply f c args)
    (throw (ex-info (str "No method " method " in " (keys (meta c)))
                    {:state @c
                     :meta (meta c)}))))

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

(defdispatch -key-pressed [this]
  (let [keymap (:key-pressed-map (meta this))]
    (when-let [f (get keymap (q/key-as-keyword)
                      (get keymap (q/key-code)))]
      (f this))))

(defdispatch -key-released [this])
(defdispatch -key-typed [this])

(defdispatch on-model-changed [this old new]
  (when (not= old new)
    (mark-dirty! this)))

(defn delegate [c f & args]
  (let [cval (with-meta @c (meta c))]
    (if-let [props (:props cval)]
      (with-props props
        (apply f cval args))
      (apply f cval args))))

(defn draw [c x y w h]
  (alter-meta! c assoc :parent *drawing-component*)
  (let [root (find-root c)]
    (when (and (:focusable? (meta c))
               (not (:focused (meta root))))
      (alter-meta! root assoc :focused c)))
  (binding [*drawing-component* c]
    (delegate c -draw x y w h))
  (swap! c assoc :bounds [x y w h])
  (add-watch c ::rerender (fn [k r o n] (on-model-changed c o n))))

(defn min-size [c] (delegate c -min-size))
(defn max-size [c] (delegate c -max-size))
(defn pref-size [c] (delegate c -pref-size))

(defn text-draw [{:keys [text props]} x y w h]
  (let [tw (q/text-width text)
        th (prop :text-size 12)]
    (q/text text
            (max x (+ x (/ w 2) (- (/ tw 2))))
            (max y (+ y (/ h 2) (- (/ th 2))))
            w
            h)))

(defn text-min-size [{:keys [text]}]
  [(q/text-width text) (prop :text-size 12)])

(defn text-pref-size [{:keys [text]}]
  (let [text-size (prop :text-size 12)]
    [(+ (q/text-width text) text-size)
     (* 2 text-size)]))

(defn outline-draw [{:keys [child props]} x y w h]
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

(defn outline-delegate-size [size-f]
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
    [Long/MAX_VALUE Long/MAX_VALUE]))

(defn dispatch-mouse-event [c e]
  ;; dispatch both a generic `:-mouse-event`, and a specific event e.g.
  ;; `:-mouse-clicked`. Components can implement one or the other. The generic
  ;; event is convenient for implementing forwarding logic.
  (dispatch c :-mouse-event e)
  (dispatch c (keyword (str "-" (name (:type e)))) e))

(defn forward-mouse-event [parent e]
  (let [{:keys [child children]} @parent
        {:keys [x y type]} e]
    (doseq [c (if child [child] children)
            :let [[cx cy cw ch] (:bounds @c)]]
      (when (and (<= cx x (+ cx cw))
                 (<= cy y (+ cy ch)))
        (prn (meta c) e)
        (dispatch-mouse-event c e)))))

;;;;;;;;;;;;;;;
;; API

(def text-meta
  {:-draw      #'text-draw
   :-min-size  #'text-min-size
   :-pref-size #'text-pref-size})

(defn text [text & {:as props}]
  (atom {:text text :props props}
        :meta text-meta))

(def outline-meta
  {:-draw        #'outline-draw
   :-min-size    (outline-delegate-size min-size)
   :-pref-size   (outline-delegate-size pref-size)
   :-mouse-event #'forward-mouse-event})

(defn outline [child & {:as props}]
  (atom {:text child :props props}
        :meta outline-meta))

(def container-meta
  {:-draw        #'container-draw
   :-min-size    #'container-size
   :-pref-size   #'container-size
   :-max-size    #'container-size
   :-mouse-event #'forward-mouse-event})

(defn container [children layout-f]
  (atom {:children children
         :layout-f layout-f}
        :meta container-meta))

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

(def select-list-meta
  {:-draw        #'select-list-draw
   :-min-size    #'container-size
   :-pref-size   #'container-size
   :-max-size    #'container-size
   :focusable?   true
   :key-pressed-map
   {:down
    (fn [this]
      (swap! this update :index
             (fn [i]
               (min (inc i) (dec (count (:children @this))))))
      (dispatch this :on-index-changed (:index @this)))
    :up
    (fn [this]
      (swap! this update :index
             (fn [i]
               (max 0 (dec i))))
      (dispatch this :on-index-changed (:index @this)))}})

(defn select-list [children]
  (atom {:children   children
         :layout-f   layout-stack
         :index      0}
        :meta select-list-meta))

(defn hslider-draw [{:keys [min max step value
                            height color bg-color]} x y w h]
  (with-props {:stroke-weight 1}
    (with-props {:fill bg-color}
      (q/rect x y w h))
    (with-props {:fill color}
      (q/rect x y (/ (* w value) (- max min)) h)))
  (with-props {:text-align :center}
    (q/text (str value)
            (+ x (/ w 2))
            (+ y (/ h 2) (/ (prop :text-size) 3)))))

(defn hslider-size [c]
  [Long/MAX_VALUE (:height c)])

(defn hslider-mouse-pressed [c {:keys [x y]}]
  (swap! c
         (fn [{:keys [bounds min max] :as cv}]
           (let [[cx _ cw _] bounds]
             (assoc cv :value
                    (* (- max min)
                       (/ (- x cx) cw)))))))

(def hslider-meta
  {:-draw          #'hslider-draw
   :-pref-size     #'hslider-size
   :-mouse-pressed #'hslider-mouse-pressed
   :-mouse-dragged #'hslider-mouse-pressed
   })

(defn hslider [{:keys [min max step value
                       height color]}]
  (atom
   {:min (or min 0)
    :max max
    :step step
    :value (or value min 0)
    :height (or height 30)
    :color (or color [73 175 157])}
   :meta
   hslider-meta))

;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn draw-root [c]
  (when (or (:dirty? (meta c))
            (not= [0 0 (q/width) (q/height)] (:bounds @c)))
    (alter-meta! c assoc :dirty? false)
    (set-prop! :background (prop :background))
    (draw c 0 0 (q/width) (q/height))))

(defn mouse-event-handler [c t]
  (dispatch-mouse-event
   c
   {:type t
    :x (q/mouse-x)
    :y (q/mouse-y)
    :button (q/mouse-button)
    :pressed? (q/mouse-pressed?)}))

(defn mouse-wheel-event-handler [c i]
  (dispatch-mouse-event
   c
   {:type :mouse-wheel
    :x (q/mouse-x)
    :y (q/mouse-y)
    :button (q/mouse-button)
    :pressed? (q/mouse-pressed?)
    :wheel-rotation i}))

(defn middleware [{::keys [root defaults] :as options}]
  (assoc options
         :setup          #(init-props! defaults)
         :draw           #(with-props defaults (draw-root @root))
         :key-pressed    #(-key-pressed (:focused (meta @root)))
         :key-released   #(-key-released (:focused (meta @root)))
         :key-typed      #(-key-typed (:focused (meta @root)))
         :key-typed      #(-key-typed (:focused (meta @root)))
         :mouse-entered  #(mouse-event-handler @root :mouse-entered)
         :mouse-exited   #(mouse-event-handler @root :mouse-exited)
         :mouse-pressed  #(mouse-event-handler @root :mouse-pressed)
         :mouse-released #(mouse-event-handler @root :mouse-released)
         :mouse-clicked  #(mouse-event-handler @root :mouse-clicked)
         :mouse-moved    #(mouse-event-handler @root :mouse-moved)
         :mouse-dragged  #(mouse-event-handler @root :mouse-dragged)
         :mouse-wheel    #(mouse-wheel-event-handler @root %1)))
