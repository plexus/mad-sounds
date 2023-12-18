(ns mad-sounds.sessions.aarschot-2023-12-17-sample-picker
  (:require
   [casa.squid.plasticine :as p]
   [clojure.java.io :as io]
   [lambdaisland.data-printers :as dp]
   [quil.core :as q])
  (:import
   (org.freedesktop.gstreamer Gst Version)))

(defn ls [& path]
  (seq (.listFiles (apply io/file "/home/arne/Sounds/Home Made Drum Kit #1" path))))

(dp/register-print clojure.lang.Atom "atom" deref)
(dp/register-pprint clojure.lang.Atom "atom" deref)

(Gst/init Version/BASELINE)


(defn enter-directory [selector dir]
  (let [dir (io/file dir)
        files (cons (io/file dir "..") (.listFiles dir))]
    (doto selector
      (swap! assoc
             :index 0
             :dir dir
             :children (for [f files]
                         (p/text-c (str (when (.isDirectory f) "[DIR] ") (.getName f)))))
      (alter-meta! assoc-in [:key-pressed-map 10]
                   (fn [this]
                     (let [f (nth files (:index @this))]
                       (if (.isDirectory f)
                         (enter-directory this f)
                         (.play (Gst/parseLaunch (str "playbin uri=" (pr-str (str (.toURI f))))))
                         ))))
      (alter-meta! assoc :on-index-changed
                   (fn [this idx]
                     (let [f (nth files idx)]
                       (if (and (.isFile f) (re-find #"\.(wav|flac|mp3)$" (.getName f)))
                         (.play (Gst/parseLaunch (str "playbin uri=" (pr-str (str (.toURI f))))))
                         )))))))

(defn file-selector [dir]
  (enter-directory (p/select-list []) dir))

(def defaults
  {:text-size 25
   :frame-rate 30
   :stroke [0 0 0]
   :fill [0 0 0]
   :stroke-weight 15
   :background [235 214 125]
   :rect-mode :corner
   :stroke-cap :round})

(defn setup []
  (p/init-props! defaults))

(meta app)

(def app
  (p/mark-dirty!
   (file-selector  "/home/arne/Sounds/Home Made Drum Kit #1")))

(defn draw-fn []
  (p/with-props defaults
    (p/draw-root app)))

(defn key-pressed-fn [root] (p/-key-pressed (:focused (meta root))))
(defn key-released-fn [root] (p/-key-released (:focused (meta root))))
(defn key-typed-fn [root] (p/-key-typed (:focused (meta root))))

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
