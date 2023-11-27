(ns jack-auto-connect
  (:require
   [vibeflow.midi.jack :as jack])
  (:import
   (org.jaudiolibs.jnajack Jack)))

(def client (jack/client :vibeflow))
