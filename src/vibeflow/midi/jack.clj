(ns vibeflow.midi.jack
  "Wrapper for Jack (Jack Audio Connection Kit) midi

  Allows creating in/out ports, and registering callbacks of various types.

  Callback name + argument list of callback function

  - :process [client frames]
  - :buffer-size-changed [client buffersize]
  - :client-registered [client name]
  - :client-unregistered [client name]
  - :ports-connected [client port-name-1 port-name-2]
  - :ports-disconnected [client port-name-1 port-name-2]
  - :port-registered [client port-name]
  - :port-unregistered [client port-name]
  - :sample-rate-changed [client rate]
  - :client-shutdown [client]
  - :update-position [client state frame pos new-pos]

  See [[register]]/[[unregister]], which work analogous
  to [[add-watch!]] (idempotent, REPL safe, etc).
  "
  (:require
   [clojure.pprint :as pprint])
  (:import
   (org.jaudiolibs.jnajack
    Jack
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
   (java.util EnumSet)))

(set! *warn-on-reflection* true)

;; The jack docs say to reuse a single instance... Might want to make this
;; thread-local or lock on it.
(defonce ^JackMidi$Event global-midi-event (JackMidi$Event.))
(defonce ^JackPosition global-jack-pos (JackPosition.))

(defonce !instance (delay (Jack/getInstance)))

(defn instance ^Jack [] @!instance)

(defonce clients (atom {}))

(defprotocol Registry
  (register [this type key val])
  (unregister [this type key])
  (lookup [this type key]))

(defrecord JackClientWrapper [^JackClient client registry]
  Registry
  (register [_ t k val]
    (swap! registry assoc-in [t k] val))
  (unregister [_ t k]
    (swap! registry update t dissoc k))
  (lookup [_ t k]
    (get-in @registry [t k]))

  clojure.lang.IDeref
  (deref [_]
    @registry)

  Object
  (toString [_]
    (str "#<JackClientwrapper " (.getName client) ">")))

(defmacro registry-callback [client registry cb-type set-cb-type & methods]
  `(~(symbol (str "." set-cb-type))
    ~client
    (reify ~(symbol (str "Jack" cb-type "Callback"))
      ~@(for [[method k args] (partition 3 methods)]
          `(~method ~(into '[_] args)
            (reduce (fn [ok?# [k# cb#]]
                      ;; If a callback returns falsy, then any remaining
                      ;; callbacks of that type are skipped
                      (when ok?#
                        (try
                          (cb# ~@args)
                          (catch Exception e#
                            (println "Error in" '~method "callback" k#)
                            (println e#)
                            ok?#))))
                    true
                    (~k @~registry)))))
    ~@(drop (- (count methods) (mod (count methods) 3)) methods)))

(defmethod print-method JackClientWrapper [x ^java.io.Writer writer]
  (.append writer (.toString ^Object x)))

(defmethod pprint/simple-dispatch JackClientWrapper [x]
  (.write ^java.io.Writer *out* (.toString ^Object x)))

;; (macroexpand-1 '(registry-callback client registry ClientRegistration
;;                                    clientRegistered :client-registered [client name]
;;                                    clientUnregistered :client-unregistered [client name]))

;; (require 'clojure.reflect)

;; (clojure.reflect/reflect
;;  JackBufferSizeCallback)

(defn make-client
  "Construct a new Jack client. Prefer [[client]] which is idempotent."
  [name]
  (let [status (EnumSet/noneOf JackStatus)
        client (.openClient (instance)
                            name
                            (EnumSet/of JackOptions/JackNoStartServer)
                            status)
        registry (atom {})]
    (when (seq status)
      (println "make-client:" (map str status)))
    ;; TODO: set these callbacks up when the first handler of that type is
    ;; registered
    (registry-callback client registry Process setProcessCallback
                       process :process [client frames])
    (registry-callback client registry BufferSize setBuffersizeCallback
                       buffersizeChanged :buffer-size-changed [client buffersize])
    (registry-callback client registry ClientRegistration setClientRegistrationCallback
                       clientRegistered :client-registered [client name]
                       clientUnregistered :client-unregistered [client name])
    (registry-callback client registry PortConnect setPortConnectCallback
                       portsConnected :ports-connected [client port-name-1 port-name-2]
                       portsDisconnected :ports-disconnected [client port-name-1 port-name-2])
    (registry-callback client registry PortRegistration setPortRegistrationCallback
                       portRegistered :port-registered [client port-name]
                       portUnregistered :port-unregistered [client port-name])
    (registry-callback client registry SampleRate setSampleRateCallback
                       sampleRateChanged :sample-rate-changed [client rate])
    (registry-callback client registry Shutdown onShutdown
                       clientShutdown :client-shutdown [client])

    ;; Immediately throws, not clear why
    ;; (registry-callback client registry Sync setSyncCallback
    ;;                    syncPosition :sync-position [client position state])
    (registry-callback client registry GraphOrder setGraphOrderCallback
                       graphOrderChanged :graph-order-changed [client])

    (.activate client)
    (let [c (->JackClientWrapper client registry)]
      (swap! clients assoc name c)
      c)))

(defn make-time-master
  ([client]
   (make-time-master client false))
  ([client force?]
   (let [{:keys [client registry]} client]
     (registry-callback ^JackClient client registry Timebase setTimebaseCallback
                        updatePosition :update-position [client state frame pos new-pos]
                        (not force?)))
   client))

(defn client
  "Get a client for a given name, creating it if it doesn't exist."
  [client-name]
  (let [name (if (keyword? client-name)
               (subs (str client-name) 1)
               client-name)]
    (or (get @clients name) (make-client name))))

(defn midi-port [^JackClientWrapper client name type]
  (assert (keyword? name))
  (if-let [port (lookup client type name)]
    port
    (let [port (.registerPort ^JackClient (:client client)
                              (subs (str name) 1)
                              JackPortType/MIDI
                              (case type
                                :midi/input JackPortFlags/JackPortIsInput
                                :midi/output JackPortFlags/JackPortIsOutput))]
      (register client type name port)
      port)))

(defn midi-in-port
  "Get a midi input name for a given client with a given name. Idempotent."
  [client name]
  (midi-port client name :midi/input))

(defn midi-out-port
  "Get a midi output name for a given client with a given name. Idempotent."
  [client name]
  (midi-port client name :midi/output))

(defn read-midi-event [port idx]
  #_(locking global-midi-event)
  (JackMidi/eventGet global-midi-event port idx)
  (let [msg (byte-array (.size global-midi-event))]
    (.read global-midi-event msg)
    [msg (.time global-midi-event)]))

(defn read-midi-events
  "Read midi events that happened in this processing cycle for a given input port.
  Call within a processing callback."
  [port]
  (doall
   (for [idx (range (JackMidi/getEventCount port))]
     (read-midi-event port idx))))

(defn write-midi-event [port time msg]
  (JackMidi/eventWrite port time msg (count msg)))

(defn filter-pipe
  "Helper for creating midi filters, forward all messages from `in` to `out` if
  they satisfy `pred`."
  [in out pred]
  (try
    (JackMidi/clearBuffer out)
    (dotimes [idx (JackMidi/getEventCount in)]
      (let [[msg time] (read-midi-event in idx)]
        (when (pred msg)
          (write-midi-event out time msg))))
    true
    (catch JackException e
      (println "JackException:" e)
      true)))

(defn start-transport! [client]
  (.transportStart ^JackClient (:client client)))

(defn stop-transport! [client]
  (.transportStop ^JackClient (:client client)))

(def port-flags
  {:can-monitor JackPortFlags/JackPortCanMonitor
   :in JackPortFlags/JackPortIsInput
   :out JackPortFlags/JackPortIsOutput
   :physical JackPortFlags/JackPortIsPhysical
   :terminal JackPortFlags/JackPortIsTerminal})

(defn ports
  "Get a vector of Jack ports (strings). Optionally takes a set of keywords to
  filter by type and port flags, e.g. #{:midi :out}, #{:audio :physical}"
  ([client]
   (into [] (.getPorts (instance) (:client client) nil nil nil)))
  ([client type-and-flags]
   (into
    []
    (.getPorts (instance) (:client client) nil
               (cond
                 (every? type-and-flags [:midi :audio])
                 nil
                 (:audio type-and-flags)
                 JackPortType/AUDIO
                 (:midi type-and-flags)
                 JackPortType/MIDI
                 :else nil)
               (let [flags (EnumSet/noneOf JackPortFlags)]
                 (doseq [[kw flag] port-flags]
                   (when (get type-and-flags kw)
                     (.add flags flag)))
                 flags)))))

(comment
  (ports (client :vibeflow) #{:midi :in}))

(defn connections
  ([client]
   (for [from (ports client #{:out})
         to (connections client from)]
     [from to]))
  ([client port]
   (into [] (.getAllConnections (instance) (:client client) port))))

(defn connect [client from to]
  (.connect (instance) (:client client) from to))

(defn disconnect [client from to]
  (.disconnect (instance) (:client client) from to))

(defn connect!
  "Sets jack connections to exactly the given connections, given as a list of
  pairs (port names, String)."
  [client conns]
  (let [ports (ports client)
        existing (set (for [from ports
                            to (connections client from)]
                        (vec (sort [from to]))))]
    (doseq [[from to] existing]
      (when-not (some #{[from to] [to from]} conns)
        (try
          (disconnect client to from)
          (catch Exception _))))
    (doseq [[from to] conns]
      (when-not (some #{[from to] [to from]} existing)
        (when (and (some #{from} ports) (some #{to} ports))
          (try
            (connect client from to)
            (catch Exception _)))))))

(defn pos->map [^JackPosition pos]
  {:bar (.getBar pos)
   :beat (.getBeat pos)
   :tick (.getTick pos)
   :ticks-per-beat (.getTicksPerBeat pos)
   :frame (.getFrame pos)
   :frame-rate (.getFrameRate pos)
   :bpm (.getBeatsPerMinute pos)
   :beats-per-bar (.getBeatsPerBar pos)
   :beat-type (.getBeatType pos)})

(defn transport-pos [client]
  (let [state (.transportQuery ^JackClient (:client client) global-jack-pos)]
    (assoc
      (pos->map global-jack-pos)
      :state
      (cond
        (= state JackTransportState/JackTransportStopped)     :stopped
        (= state JackTransportState/JackTransportRolling)     :rolling
        (= state JackTransportState/JackTransportLooping)     :looping
        (= state JackTransportState/JackTransportStarting)    :starting
        (= state JackTransportState/JackTransportNetStarting) :net-starting ;; Waiting for sync ready on the network
        ))))
