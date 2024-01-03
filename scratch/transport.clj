(ns overtone.studio.transport
  (:require [clojure.pprint :as pprint]
            [overtone.music.rhythm :as rhythm]))

(def DEFAULT-BPM 128)
(def DEFAULT-BEATS-PER-BAR 4)
(def DEFAULT-BEATS-TYPE 4)
(def DEFAULT-TICKS-PER-BEAT 1920.0)

;; (def ^:dynamic *clock* (rhythm/metronome DEFAULT-BPM))

;; Mutable to allow reuse in the main processing loop, we want to avoid creating
;; constant garbage
(deftype Position
    [;; Frame based position
     ;; ^:volatile-mutable ^long frame
     ;; ^:volatile-mutable ^long frame-rate
     ;; ^:volatile-mutable ^double frames-per-tick
     ;; BBT based position
     ^:volatile-mutable ^long bar
     ^:volatile-mutable ^long beat
     ^:volatile-mutable ^double tick
     ^:volatile-mutable ^double bar-start-tick
     ;; Signature
     ^:volatile-mutable ^double beats-per-bar
     ^:volatile-mutable ^double beat-type
     ;; Tempo
     ^:volatile-mutable ^double ticks-per-beat
     ^:volatile-mutable ^double beats-per-minute]

  clojure.lang.ILookup
  (valAt [this k]
    (case k
      ;; :frame frame
      ;; :frame-rate frame-rate
      ;; :frames-per-tick frames-per-tick
      :bar bar
      :beat beat
      :tick tick
      :bar-start-tick bar-start-tick
      :beats-per-bar beats-per-bar
      :beat-type beat-type
      :ticks-per-beat ticks-per-beat
      :beats-per-minute beats-per-minute))
  (valAt [this k fallback]
    (or (.valAt this k) fallback))

  clojure.lang.ITransientAssociative
  (assoc [this k v]
    (case k
      ;; :frame (set! frame (long v))
      ;; :frame-rate (set! frame-rate (long v))
      ;; :frames-per-tick (set! frames-per-tick (double v))
      :bar (set! bar (long v))
      :beat (set! beat (long v))
      :tick (set! tick (double v))
      :bar-start-tick (set! bar-start-tick (double v))
      :beats-per-bar (set! beats-per-bar (double v))
      :beat-type (set! beat-type (double v))
      :ticks-per-beat (set! ticks-per-beat (double v))
      :beats-per-minute (set! beats-per-minute (double v)))
    this)

  ;; Deref to "snapshot"
  clojure.lang.IDeref
  (deref [this]
    (array-map
     ;; :frame frame
     ;; :frame-rate frame-rate
     ;; :frames-per-tick frames-per-tick
     :bar bar
     :beat beat
     :tick tick
     :bar-start-tick bar-start-tick
     :beats-per-bar beats-per-bar
     :beat-type beat-type
     :ticks-per-beat ticks-per-beat
     :beats-per-minute beats-per-minute)))

(defmethod print-method Position [this ^java.io.Writer writer]
  (.write writer "#overtone/position ")
  (print-method @this writer))

(defmethod pprint/simple-dispatch Position [this]
  (print "#overtone/position ")
  (pprint/pprint @this))

(defn map->Position [{:keys [;; frame
                             ;; frame-rate
                             ;; frames-per-tick
                             bar
                             beat
                             tick
                             bar-start-tick
                             beats-per-bar
                             beat-type
                             ticks-per-beat
                             beats-per-minute]
                      :or {;; frame 0
                           ;; frame-rate 44100
                           ;; frames-per-tick 0
                           bar 0
                           beat 0
                           tick 0
                           bar-start-tick 0
                           beats-per-bar DEFAULT-BEATS-PER-BAR
                           beat-type DEFAULT-BEATS-TYPE
                           ticks-per-beat DEFAULT-TICKS-PER-BEAT
                           beats-per-minute DEFAULT-BPM}}]
  (->Position
   ;; frame frame-rate frames-per-tick
   bar beat tick bar-start-tick
   beats-per-bar beat-type
   ticks-per-beat beats-per-minute))

(def position (map->Position {}))

(def ^:dynamic transport-provider nil)
