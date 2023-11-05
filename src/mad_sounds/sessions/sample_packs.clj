(ns mad-sounds.sessions.sample-packs
  (:require
   [clojure.java.io :as io]
   [overtone.sc.sample :as samp]
   [overtone.samples.freesound :as freesound]))

;; AMEN drum kit
;; https://freesound.org/people/VEXST/packs/1655/
;; CC-by
(freesound/freesound-pack-info 1655)

(defn freesound-sample-pack [id]
  (into
   {}
   (for [sample-file (file-seq (io/file (freesound/freesound-pack-dir id)))
         :let [[_ id user sample-name] (re-find #"/(\d+)__([^/\.]+)__([^\.]+).wav" (str sample-file))]
         :when sample-name]
     [(keyword sample-name)
      (freesound/map->FreesoundSample
       (assoc (samp/load-sample sample-file)
              :sample-name sample-name
              :freesound-id (Long/parseLong id)))])))

(overtone.core/demo 1
                    (overtone.core/play-buf 2
                                            (:id (:bass-stab
                                                  (freesound-sample-pack 1655)))))

(:freesound-id
 (:bass-stab
  (freesound-sample-pack 1655)))
;; =>



(
 (:bass-stab
  (freesound-sample-pack 1655))
 :loop? false
 :release 5.1)
