(ns mad-sounds.sessions.looptober-2024-10-18
  (:use overtone.live)
  (:require [casa.squid.jack :as jack]))

(demo (sin-osc))

(jack/connect!
 #{["Overtone:out_1" #{"Family 17h/19h HD Audio Controller Pro:playback_AUX0"}]
   ["Overtone:out_2" #{"Family 17h/19h HD Audio Controller Pro:playback_AUX1"}]
   })

(def wolves (sample "samples/wolves.wav"))

(definst slidinator [buf-id 0
                     depth 500
                     freq 1
                     start 0
                     end 1
                     amp 1
                     gate 1]
  (let [samples (buf-frames buf-id)
        start (+ depth (* start samples))
        end  (* end (- samples depth))
        dur (/ (- end start) (sample-rate))
        pos (phasor:ar :start start :end end)]
    (* amp
       (env-gen (envelope [0 1 0] [0.01 dur] :linear 2) gate :action FREE)
       (env-gen (envelope [0 1 1 0] [0.01 (- dur 0.02) 0.01]))
       (buf-rd 2 buf-id (lin-lin (var-saw freq)
                                 -1 1
                                 (- pos depth) (+ pos depth))))))

(do
  (slidinator wolves :depth 1 :freq 1)
  (dotimes [i 300]
    (ctl slidinator :depth i :freq i)
    (Thread/sleep 20)))
(stop)

inst-fx!

(slidinator wolves :depth 300 :freq 300)

(into {} slidinator)
