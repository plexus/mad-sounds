(ns vibeflow.drumkv1
  (:require [clojure.data.xml :as xml]
            [net.cgrand.enlive-html :as enlive]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def kit (-> "/home/arne/Music/Various Famous Drum Kits/Living Legend Sound Kit/LIVING LEGEND KIT/LIVING LEGEND KIT.kit"
             io/file
             io/reader
             xml/parse))

(defn read-kit [f]
  (-> f io/file slurp (str/replace #"&" "&amp;") xml/parse-str))

(defn kit-data [kit]
  {:name (:name (:attrs kit))
   :slots
   (for [slot (enlive/select kit [:SampleSlot])]
     {:key (:rootKey (:attrs slot))
      :samples (for [sample (enlive/select slot [:Sample])]
                 {:file (:file (:attrs sample))})})})

(defn drumkit [{:keys [name slots]}]
  [:preset {:name name :version "0.9.12"}
   [:elements
    (for [{:keys [key samples]} slots]
      [:element {:index key}
       (for [[{:keys [file]} idx] (map list samples (range))]
         [:sample {:name "GEN1_SAMPLE" :index idx}
          file])])]])

(comment
  (doseq [kit-file (filter #(.endsWith (str %) ".kit")
                           (file-seq (io/file "/home/arne/Music/Various Famous Drum Kits/")))]
    (try
      (spit (str/replace (str kit-file) #"kit$" "drumkv1")
            (xml/emit-str
             (enlive/html
              (drumkit (kit-data (read-kit kit-file))))
             :doctype "<!DOCTYPE drumkv1>"))
      (catch Exception e
        (println kit-file)))))

(defn ext? [ext]
  #(.endsWith (str %) (str "." ext)))

(defn parent-dir [f]
  (.getParent (io/file f)))

(defn dir-kit-data [kit-dir]
  (let [wavs (filter (ext? "wav") (file-seq kit-dir))]
    {:name (.getName kit-dir)
     :slots
     (for [[wav idx] (map list wavs (range 36 Long/MAX_VALUE))]
       {:key idx
        :samples [{:file (str wav)}]})}))

(let [files (file-seq (io/file "/home/arne/Music/Various Famous Drum Kits/"))
      wav-dirs (into #{}
                     (comp
                      (filter (ext? "wav"))
                      (map parent-dir))
                     files)
      kit-dirs (into #{}
                     (comp
                      (filter (ext? "kit"))
                      (map parent-dir))
                     files)
      missing-kits (remove kit-dirs wav-dirs)]
  (def kit-dir (io/file (first missing-kits))))

(bean kit-dir)

(filter (ext? "wav") (file-seq kit-dir))



(dir-kit-data kit-dir)

(take 3 (range 36 Long/MAX_VALUE))
