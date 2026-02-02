(ns mad-sounds.sessions.aarschot-2026-01-31-nsm-shizzle
  (:require
   [overtone.osc :as osc]
   [overtone.osc.peer :as peer])
  (:import
   (java.lang ProcessHandle)))

;; osc.udp://bubblegum:16187/

(def client (peer/client-peer "localhost" 16187))
(osc/osc-debug)

(osc/osc-send client "/nsm/server/announce"
              "vibeflow" "dirty" "vibeflow" "1" "1" (.pid (ProcessHandle/current)))
s:application_name s:capabilities s:executable_name i:api_version_major i:api_version_minor i:pid
