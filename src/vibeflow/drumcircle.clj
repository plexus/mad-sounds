(ns vibeflow.drumcircle
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
  {:pattern #{[0 :kick]
              [1/4 :snare]
              [2/4 :kick]
              [5/8 :snare]
              [3/4 :kick]}
   :instruments {:kick [9 35 127] ;; [channel note velocity]
                 :snare [9 38 127]
                 :hi-hat [9 42 127]
                 :hi-hat-open [9 46 127]}
   :bar 0
   :beat 0
   :tick 0
   :ticks-per-beat 1920
   :frame 0
   :frame-rate 44100
   :playing? false
   :bpm 120
   :beats-per-bar 4 ;; Together these
   :beat-type 4     ;; form the time signature (4/4)
   })

(defonce state (atom init-state))

;; (reset! state init-state)

(defn note-in-range [^double n ^double start ^double end]
  (or (<= start n end)
      (and (< 1 end)
           (or (<= start n)
               (<= n (- end 1))))))

(defn note-offset [^double n ^double start]
  (if (< n start)
    (- (+ n 1) start)
    (- n start)))

;; bar, beat : start at 1
;; tick : starts at 0

(defn sample-at-bbt [{:keys [bpm beats-per-bar beat-type
                             frame-rate ticks-per-beat]}
                     bar beat tick]
  (if (or (zero? ticks-per-beat)
          (zero? bpm))
    0
    (let [beats (+ (* (dec bar) beats-per-bar)
                   (dec beat)
                   (/ tick ticks-per-beat))]
      (* 60 (/ beats bpm) frame-rate))))

(defn bbt-at-sample [{:keys [bpm beats-per-bar beat-type
                             frame-rate ticks-per-beat]}
                     sample]
  (let [beats (* (/ sample frame-rate 60) bpm)
        bars  (quot beats beats-per-bar)
        ticks (* (mod beats 1) ticks-per-beat)
        beats (quot beats 1)]
    [(inc bars) (inc beats) ticks]))

(defn format-bbt [bbt]
  (str/join "|" (map long bbt)))

(defn fraction->beat-tick [f]
  (let [ticks (* 4 1920 f)]
    [(inc (quot ticks 1920)) (mod ticks 1920)]))

(defn preceding
  "Seek back from the current bar|beat|tick for the previous occurance of the
  given beat+tick."
  [[bar beat tick] [b t]]
  (if (or (< b beat)
          (and (= b beat) (< t tick)))
    [(dec bar) b t]
    [bar b t]))

(defn succeeding
  "Seek forward from the current bar|beat|tick for the next occurance of the
  given beat+tick."
  [[bar beat tick] [b t]]
  (if (or (< b beat)
          (and (= b beat) (< t tick)))
    [(inc bar) b t]
    [bar b t]))

(defn write-cycle-beats [client port cycle-frames
                         {:keys [bar beat tick ticks-per-beat
                                 frame frame-rate
                                 bpm beats-per-bar beat-type
                                 playing?
                                 pattern instruments]}]
  (let [timing {:bpm bpm
                :beats-per-bar beats-per-bar
                :beat-type beat-type
                :frame-rate frame-rate
                :ticks-per-beat ticks-per-beat}
        note-length 1/32
        note-ends (map (fn [[frac inst]]
                         (let [[bar beat tick] (preceding [bar beat tick]
                                                          (fraction->beat-tick (mod (+ frac note-length) 1)))]
                           [(sample-at-bbt timing bar beat tick) inst]))
                       pattern)
        note-starts (map (fn [[frac inst]]
                           (let [[bar beat tick] (succeeding [bar beat tick]
                                                             (fraction->beat-tick frac))]
                             [(sample-at-bbt timing bar beat tick) inst]))
                         pattern)
        cycle-start frame
        cycle-end (+ frame cycle-frames)]
    (doseq [[sample inst] note-ends
            :when (and (<= cycle-start sample)
                       (< sample cycle-end))]
      (let [[chan note velocity] (get instruments inst)]
        #_(println "OFF" inst (format-bbt (bbt-at-sample timing sample)))
        (jack/write-midi-event port
                               (- sample cycle-start)
                               (midi/message chan :note-off note 0))))

    (doseq [[sample inst] note-starts
            :when (and (<= cycle-start sample)
                       (< sample cycle-end))]
      (let [[chan note velocity] (get instruments inst)]
        #_        (println "ON" inst (format-bbt (bbt-at-sample timing sample)))
        (jack/write-midi-event port
                               (- sample cycle-start)
                               (midi/message chan :note-on note velocity))))))
#_
(defn write-cycle-beats [client port cycle-frames
                         {:keys [bar beat tick ticks-per-beat
                                 frame frame-rate
                                 bpm beats-per-bar beat-type
                                 playing?
                                 pattern instruments]}]
  (let [resolution 2
        frames-per-tick (/ frame-rate ticks-per-beat)
        ticks-per-cycle (/ (* ticks-per-beat bpm cycle-frames)
                           60
                           frame-rate)
        ;; these are "normalized" as a fraction of beat-type beats e.g.
        ;; cycle-start 0.75 with a 4/4 signature means we are on the 3rd beat
        cycle-start (+ (/ (+ (- beat 1) (/ tick ticks-per-beat)) beat-type))
        cycle-end (+ (/ (+ (- beat 1) (/ (+ tick ticks-per-cycle) ticks-per-beat)) beat-type))
        note-type (Math/round (* beat-type resolution))
        frames-per-bar (* (/ bpm 60) frame-rate)]

    (doseq [[fraction inst] pattern
            :let [end (+ fraction 1/16)]
            :when (note-in-range (if (< 1 end) (- end 1) end)
                                 cycle-start
                                 cycle-end)]
      (let [[chan note velocity] (get instruments inst)
            offset (* frames-per-bar (note-offset end cycle-start))]
        (jack/write-midi-event port
                               (long offset)
                               (midi/message chan :note-off note 0))))

    (doseq [[fraction inst] pattern
            :when (note-in-range fraction cycle-start cycle-end)]
      (let [[chan note velocity] (get instruments inst)
            offset (* frames-per-bar (note-offset fraction cycle-start))]
        (jack/write-midi-event port
                               (long offset)
                               (midi/message chan :note-on note velocity))))))

(defn assoc-pos [state ^JackPosition pos]
  (assoc state
         :bar (.getBar pos)
         :beat (.getBeat pos)
         :tick (.getTick pos)
         :ticks-per-beat (.getTicksPerBeat pos)
         :frame (.getFrame pos)
         :frame-rate (.getFrameRate pos)
         :bpm (.getBeatsPerMinute pos)
         :beats-per-bar (.getBeatsPerBar pos)
         :beat-type (.getBeatType pos)))

(defn start-sequencer []
  (let [client (jack/client :vibeflow)
        midi-out (jack/midi-out-port client :drumcircle)
        jack-pos (JackPosition.)]
    (jack/register
     client :process ::my-process
     (fn [client cycle-frames]
       (let [transport-state (.transportQuery client jack-pos)
             playing? (= transport-state JackTransportState/JackTransportRolling)
             new-state (swap! state (fn [state] (assoc (assoc-pos state jack-pos) :playing? playing?)))]
         #_(print (if playing? ">" ".")) (flush)
         #_(prn @state)
         (JackMidi/clearBuffer midi-out)
         (when playing?
           (write-cycle-beats client midi-out cycle-frames new-state)))
       true))))

(defn play-pause! []
  (let [client  (jack/client :vibeflow)]
    (cond
      (= :rolling (:state (jack/transport-pos client)))
      (jack/stop-transport! client)
      (= :stopped (:state (jack/transport-pos client)))
      (jack/start-transport! client))))

(defn save-pattern [name]
  (spit (str "src/" name ".clj")
        (with-out-str
          (println ";;" name (java.util.Date.))
          (println "(require 'vibeflow.drumcircle)")
          (println)
          (pprint/pprint `(~'swap! state ~'merge ~(select-keys @state [:pattern :instruments]))))))


(comment
  (start-sequencer)
  (swap! state assoc
         :pattern
         #{[0 :kick]
           [1/4 :snare]
           [5/8 :kick]
           [3/4 :snare]

           [0 :hi-hat]
           [1/8 :hi-hat]
           [1/4 :hi-hat]
           [3/8 :hi-hat]
           [1/2 :hi-hat]
           [5/8 :hi-hat]
           [3/4 :hi-hat]
           [7/8 :hi-hat]})


  ;; https://www.youtube.com/watch?v=tm2BgO1VaRY&list=WL&index=1
  (save-pattern "four_on_the_floar")
  (load "/four_on_the_floar")

  (save-pattern "basic_rock")
  (load "/basic_rock")

  (save-pattern "levee_break")
  (load "/levee_break")

  (save-pattern "impeach_the_president")
  (load "/impeach_the_president")

  (save-pattern "funky_drummer")
  (load "/funky_drummer")
  )
