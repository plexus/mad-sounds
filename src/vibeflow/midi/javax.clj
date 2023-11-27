(ns vibeflow.midi.javax
  (:refer-clojure :exclude [send])
  (:require [vibeflow.midi.core :as midi]
            [clojure.string :as str])
  (:import (javax.sound.midi MidiDevice
                             MidiSystem
                             Receiver
                             Transmitter
                             MidiMessage
                             SysexMessage
                             MetaMessage
                             ShortMessage)))

(set! *warn-on-reflection* true)

(defprotocol MidiOps
  (receiver ^Receiver [_])
  (transmitter ^Transmitter [_]))

(defn connect [from to]
  (.setReceiver (transmitter from) (receiver to)))

(defn disconnect [^Transmitter from]
  ;; We don't coerce here since we need the actual transmitter used in connect,
  ;; so call transmitter yourself if you want to be able to disconnect.
  (.setReceiver from nil))

(defn java-midi-message
  "Convert a midi message (byte or long array) into a MidiMessage"
  [msg]
  ;; Note: 0xFF can mean reset or meta, depending on if it's sent over the wire,
  ;; or read from a file. Java uses separate classes to represent these. We
  ;; always encode it as a reset ShortMessage
  (if (= :sysex-start (midi/event-type msg))
    (SysexMessage. msg (count msg))
    (ShortMessage. (midi/status msg)
                   (midi/data1 msg)
                   (midi/data2 msg))))

(defn send [to msg timestamp]
  (.send (receiver to)
         ^MidiMessage
         (cond
           (.isArray (class msg))      (java-midi-message msg)
           (vector? msg)               (java-midi-message (midi/message msg))
           (instance? MidiMessage msg) msg)
         timestamp))

(defn device-name [^MidiDevice d]
  (.getName (.getDeviceInfo d)))

(defn receivers [^MidiDevice d]
  (.getReceivers d))

(defn transmitters [^MidiDevice d]
  (.getTransmitters d))

(defn can-transmit? [^MidiDevice d]
  ;; Can be -1 for unlimited
  (not= 0 (.getMaxTransmitters d)))

(defn can-receive? [^MidiDevice d]
  ;; Can be -1 for unlimited
  (not= 0 (.getMaxReceivers d)))

(defn query-devices []
  (map #(MidiSystem/getMidiDevice %)
       (MidiSystem/getMidiDeviceInfo)))

(defn find-input-device [name]
  (first (filter (every-pred #(str/includes? (device-name %) name)
                             can-transmit?)
                 (query-devices))))

(defn find-output-device [name]
  (first (filter (every-pred #(str/includes? (device-name %) name)
                             can-receive?)
                 (query-devices))))

(extend-protocol MidiOps
  MidiDevice
  (receiver [this]
    (.getReceiver this))
  (transmitter [this]
    (.getTransmitter this))

  Transmitter
  (transmitter [this] this)

  Receiver
  (receiver [this] this)

  clojure.lang.IFn
  (receiver [f]
    (proxy [Receiver] []
      (send [^MidiMessage message ^double timestamp]
        (f (.getMessage message) timestamp)))))

(comment
  (.open d)
  (connect d
           (fn [ts m]
             (println ts (seq m))))
  (.getMicrosecondPosition d)

  (do
    (def d (find-input-device "Mini"))
    (def o (find-output-device ""))
    (def t (transmitter d))
    (def r (receiver o))
    (.open d)
    (.open o))
  (.close d)
  (.close o)
  (connect d o)
  (.setReceiver t nil)
  )
