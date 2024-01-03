(ns overtone.studio.transport.provider)

(defprotocol TransportProvider
  (play! [this])
  (pause! [this]))
