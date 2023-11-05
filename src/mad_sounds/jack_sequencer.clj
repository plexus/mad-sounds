(ns mad-sounds.jack-sequencer
  (:import
   (org.jaudiolibs.jnajack Jack
                           JackClient
                           JackException
                           JackMidi
                           JackMidi$Event
                           JackOptions
                           JackPosition
                           JackPortFlags
                           JackPortType
                           JackProcessCallback
                           JackShutdownCallback
                           JackBufferSizeCallback
                           JackPortConnectCallback
                           JackPortRegistrationCallback
                           JackSampleRateCallback
                           JackShutdownCallback
                           JackSyncCallback
                           JackClientRegistrationCallback
                           JackTimebaseCallback
                           JackTransportState
                           JackGraphOrderCallback
                           JackStatus)
   (java.util EnumSet))
  (:require
   [vibeflow.midi.jack :as jack]
   [vibeflow.midi.core :as midi]
   [clojure.pprint :as pprint]
   [clojure.string :as str]))

(def init-state
  {:tracks {} #_{:my-inst {:params {:chan 1
                                    :vel 96}
                           :start [1 0] ; bar 4
                           :end [3 0]
                           :loop? true
                           :length [1] ; 1 bar
                           :bangs [[0]
                                   [0 2]
                                   [0 4 0 {:vel 128}]]
                           :on-bang prn}}
   :bar 0
   :beat 0
   :tick 0
   :ticks-per-beat 1920
   :bar-start-tick 0
   :frame 0
   :frame-rate 44100
   :playing? false
   :beats-per-minute 120
   :beats-per-bar 4 ;; Together these
   :beat-type 4     ;; form the time signature (4/4)
   })

(defonce state (atom init-state))

(defn -bar [bbt] (long (get bbt 0 0)))
(defn -beat [bbt] (long (get bbt 1 0)))
(defn -tick [bbt] (get bbt 2 0))

(defn bbt->ticks
  ([bbt]
   (bbt->ticks bbt @state))
  ([bbt {:keys [beats-per-bar ticks-per-beat]}]
   (+ (-tick bbt) (* ticks-per-beat (+ (-beat bbt) (* beats-per-bar (-bar bbt)))))))

(defn ticks->bbt
  ([ticks]
   (ticks->bbt ticks @state))
  ([ticks {:keys [beats-per-bar ticks-per-beat]}]
   (let [beat (quot ticks ticks-per-beat)
         tick (mod ticks ticks-per-beat)
         bar (quot beat beats-per-bar)
         beat (mod beat beats-per-bar)]
     [(long bar) (long beat) tick])))

(defn bbt-norm
  ([bbt]
   (bbt-norm bbt @state))
  ([bbt state]
   (ticks->bbt (bbt->ticks bbt state) state)))

(defn bbt< [a b]
  (let [bar1 (-bar a) beat1 (-beat a) tick1 (-tick a)
        bar2 (-bar b) beat2 (-beat b) tick2 (-tick b)]
    (or (< bar1 bar2)
        (and (= bar1 bar2) (< beat1 beat2))
        (and (= bar1 bar2) (= beat1 beat2) (< tick1 tick2)))))

(defn bbt<= [a b]
  (let [bar1 (-bar a) beat1 (-beat a) tick1 (-tick a)
        bar2 (-bar b) beat2 (-beat b) tick2 (-tick b)]
    (or (< bar1 bar2)
        (and (= bar1 bar2) (< beat1 beat2))
        (and (= bar1 bar2) (= beat1 beat2) (<= tick1 tick2)))))

(defn bbt+
  ([a b]
   (bbt+ a b @state))
  ([a b state]
   (ticks->bbt (+ (bbt->ticks a state) (bbt->ticks b state)) state)))

(defn bbt-mod
  ([num div]
   (bbt-mod num div @state))
  ([num div state]
   (ticks->bbt (mod (bbt->ticks num state) (bbt->ticks div state)) state)))

(defn frames->ticks [frames {:keys [ticks-per-beat frame-rate beats-per-minute]}]
  (*
   (/ frames frame-rate)
   (* ticks-per-beat (/ beats-per-minute 60))))

(defn write-cycle-beats [client cycle-frames
                         {:keys [bar beat tick ticks-per-beat
                                 bar-start-tick
                                 frame frame-rate
                                 beats-per-minute beats-per-bar beat-type
                                 playing?
                                 tracks]
                          :as state}]
  (when playing?
    (let [cycle-start [(dec bar) (dec beat) tick]
          cycle-end (bbt+ cycle-start [0 0 (frames->ticks cycle-frames state)])]
      (doseq [[track-key {:keys [params start end loop? length bangs on-bang]}] tracks
              :when (and on-bang
                         (or (not start) (or (bbt<= start cycle-start)
                                             (and (bbt<= cycle-start start)
                                                  (bbt<= start cycle-end))))
                         (or (not end) (bbt<= cycle-end end)))]
        (doseq [bang bangs]
          (let [bang (cond-> bang
                       start
                       (bbt+ start))
                cycle-start (cond-> cycle-start
                              (and loop? length)
                              (bbt-mod length state))
                cycle-end (cond-> cycle-end
                            (and loop? length)
                            (bbt-mod length state))]
            (doseq [[cycle-start cycle-end] (if (bbt< cycle-end cycle-start)
                                              ;; When we wrap around, process in two parts
                                              [[cycle-start (bbt+ cycle-start length state)]
                                               [[0 0 0] cycle-end]]
                                              [[cycle-start cycle-end]])]
              (when (and (bbt<= cycle-start bang)
                         (bbt< bang cycle-end))
                ;; KiSsBoP - key state bang params
                (on-bang track-key
                         (dissoc state :tracks) bang
                         (merge params (get bang 3)))))))))))

(defn assoc-pos [state ^JackPosition pos]
  (assoc state
         :bar (.getBar pos)
         :beat (.getBeat pos)
         :tick (.getTick pos)
         :ticks-per-beat (.getTicksPerBeat pos)
         :bar-start-tick (.getBarStartTick pos)
         :frame (.getFrame pos)
         :frame-rate (.getFrameRate pos)
         :beats-per-minute (.getBeatsPerMinute pos)
         :beats-per-bar (.getBeatsPerBar pos)
         :beat-type (.getBeatType pos)))

(defn init-sequencer []
  (let [client (jack/client :vibeflow)
        jack-pos (JackPosition.)]
    (jack/register
     client :process ::my-process
     (fn [client cycle-frames]
       (let [transport-state (.transportQuery client jack-pos)
             playing? (= transport-state JackTransportState/JackTransportRolling)
             new-state (swap! state (fn [state] (assoc (assoc-pos state jack-pos) :playing? playing?)))]
         #_(print (if playing? ">" ".")) (flush)
         #_(prn @state)
         (write-cycle-beats client cycle-frames new-state))
       true))))

(defn play-pause! []
  (let [client (jack/client :vibeflow)]
    (cond
      (= :rolling (:state (jack/transport-pos client)))
      (jack/stop-transport! client)
      (= :stopped (:state (jack/transport-pos client)))
      (jack/start-transport! client))))

(defn rewind! []
  (.transportLocate (:client (jack/client :vibeflow)) 0))

(defn stop! []
  (jack/stop-transport! (jack/client :vibeflow))
  (rewind!))

(defn norm-bangs [bangs]
  (for [bang bangs]
    (cond
      (number? bang)
      (bbt-norm [0 bang])
      (any? map? bang)
      (conj (bbt-norm bang) (some #(when (map? %) %) bang))
      :else
      (bbt-norm bang))))

(defn track [track-key bangs & {:as opts}]
  (swap! state assoc-in [:tracks track-key] (assoc opts :bangs (norm-bangs bangs))))

(defn _loop [track-key bangs & {:as opts}]
  (let [bangs (norm-bangs bangs)]
    (swap! state assoc-in [:tracks track-key]
           (cond-> (assoc opts
                          :bangs bangs
                          :loop? true)
             (not (:length opts))
             (assoc :length (inc (-bar (ticks->bbt (apply max (map bbt->ticks bangs))))))))))

(defn on-bang [track-key f]
  (swap! state assoc-in [:tracks track-key :on-bang] f))

(comment
  (init-sequencer))
