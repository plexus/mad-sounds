(ns vibeflow.session
  "Provides `session-atom`, acts like a normal atom, but all session atoms get
  synced to an EDN file for the current session. Call `open!` to restore/start a
  session."
  (:require
   [clojure.java.io :as io]
   [clojure.pprint :as pprint]
   [overtone.helpers.lib :as helpers-lib]
   [overtone.sc.synth :as synth]
   [overtone.studio.inst :as inst])
  (:import
   (java.util Timer TimerTask)))

(set! *warn-on-reflection* true)

(defonce session (atom {}))
(defonce atoms (atom {}))

(def ^java.io.File sessions-path (io/file "sessions"))

(defn- qualify-sym [env s]
  (when (symbol? s)
    (if (simple-symbol? s)
      (or (some-> (ns-refers *ns*) (get s) symbol)
          (symbol (str *ns*) (str s)))
      (let [ns (namespace s)
            n (name s)
            aliases (ns-aliases *ns*)]
        (symbol (or (some-> aliases (get (symbol ns)) ns-name str) ns) n)))))

(defn- debounce
  ([f] (debounce f 1000))
  ([f timeout]
   (let [timer (Timer.)
         task (atom nil)]
     (fn [& args]
       (let [new-task (proxy [TimerTask] []
                        (run []
                          (apply f args)
                          (reset! task nil)
                          (.purge timer)))
             old ^TimerTask @task]
         (when (compare-and-set! task old new-task)
           (when old (.cancel old))
           (.schedule timer new-task (long timeout))))))))

(defn- restore!
  ([data]
   (restore! ))
  ([path data]))


(defn open! [name]
  (.mkdirs sessions-path)
  (let [session-file (io/file sessions-path (str name ".edn"))
        data (if (.exists session-file)
               (read-string (slurp session-file))
               {})]
    (remove-watch session ::save)
    (add-watch
     session
     ::save
     (debounce
      (fn [k r o n]
        (spit
         (:path n)
         (with-out-str (pprint/pprint (:data n)))))))
    (reset! session
            {:name name
             :path session-file
             :data (merge
                    (update-vals @atoms deref)
                    data)})
    (doseq [[k v] data]
      (when-let [a (get @atoms k)]
        (reset! a v)))))

(defn session-atom [k & [init]]
  (let [a (get (swap! atoms (fn [as]
                              (if (get as k)
                                as
                                (assoc as k (atom (if-some [v (get-in @session [:data k])]
                                                    v
                                                    init))))))
               k)]
    (add-watch a ::sync (fn [_ _ _ n] (swap! session update :data assoc k n)))
    (swap! session update :data assoc k @a)
    a))

(defmacro defsessinst [name base-int]
  (let [full-name (qualify-sym &env name)]
    `(def ~name (inst/map->Inst
                 (-> (helpers-lib/ov-raw-map ~base-int)
                     (update :params
                             (fn [params#]
                               (for [p# params#]
                                 (let [a# (session-atom ['~full-name (:name p#)] @(:value p#))]
                                   (synth/control-proxy-value-atom '~full-name (:name p#) a#)
                                   (assoc p# :value a#))))))))))
