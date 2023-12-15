(ns mad-sounds.sessions.aarschot-2023-12-15-samples
  (:require
   [clojure.java.io :as io]
   [quil.core :as q])
  (:import
   (javax.sound.sampled AudioSystem)
   (org.freedesktop.gstreamer Gst
                              Version)))

(def sample "/home/arne/Overtone/JazzFunkKit/bop kick - snares off - 1.flac")

(AudioSystem/getAudioInputStream
 (io/file))

(seq
 (AudioSystem/getMixerInfo))

(clojure.reflect/reflect Gst)

(Gst/init Version/BASELINE)

(.play

 (Gst/parseLaunch (str "playbin uri=" (pr-str (str (.toURI (io/file sample)))))))

(Gst/parseLaunch "playbin uri=https://gstreamer.freedesktop.org/data/media/sintel_trailer-480p.webm")

(def ^:dynamic *drawing-stack* {})

(def setter
  (memoize
   (fn [k]
     (resolve (symbol "quil.core" (name k))))))

(defn with-props [props & body]
  `(let [old# *drawing-stack*
         new# ~props]
     (doseq [k# (keys new#)
             :let [v# (get new# k#)]
             :when (not= (get old# k#) v#)]
       ((setter k#) v#))
     (binding [*drawing-stack*])))

(symbol "q" (name :foo))

(defprotocol Component
  (draw [this x y w h])
  (min-size [this])
  (max-size [this])
  (pref-size [this]))

(defrecord TextC [text size])

- location
- bounds

(defn setup []
  (q/fill 0)
  (q/text-size 30)
  (q/frame-rate 30))

(defn draw []
  (q/background 192 171 121)
  (q/text "Snare.wav" 10 30)
  (q/text (str (q/text-width "Snare.wav"))
          (+ 10 (q/text-width "Snare.wav"))
          40
          ))

(q/defsketch example
  :title "Oh so many grey circles"
  :settings #(q/smooth 2) ;; Turn on anti-aliasing
  :setup setup
  :draw #'draw
  :size [323 200])
