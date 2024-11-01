(ns mad-sounds.sessions.conj-demos-2024-11-25
  (:require
   [overtone.live :refer :all]
   [vibeflow.synths :as s])
  (:import
   (java.io File FileInputStream)))

(*clock* :bpm 120)
(stop)
(ploop :x
       {:type :note
        :instrument s/marimba
        :chord-size 4
        #_#_:freq [200 400]
        #_#_:midinote [60 61 62]
        ;; :note [:c :d :e]
        :mode :mixolydian
        :degree [5 1 1]
        :amp [1 0.7 0.7]}
       {:quant 6})

(def events (atom []))

(apply map vector @events)

(defn handle-event [e]
  (swap! events conj e)
  (let [e (drop (* 5 8) e)]
    (def last-event e)
    (println (partition-all 8 e))))

(let [is (FileInputStream. (File. "/dev/input/by-id/usb-1189_8890-if02-event-kbd"))
      bs (byte-array 1024)]
  (future
    (while true
      ;; It appears we're getting 24 byte events, although for each button
      ;; press/wheel change we might get three or four of these. The first 16
      ;; bytes appear to be a timestamp, the remaining 8 we can eyeball to
      ;; find the bits that matter for us.
      (handle-event (take (.read is bs 0 1024) bs)))))

c
