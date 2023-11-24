(ns mad-sounds.sessions.aarschot-2023-11-15-sunny-day)

(require '[overtone.core :as o :refer :all])

;; pw-jack scsynth -u 55555
(o/connect-external-server 55555)


(require '[net.arnebrasseur.cljack :as jack])

(jack/ports)

(jack/connect!
 [["SuperCollider:out_1"  "AG06/AG03 Analog Stereo:playback_FL"]
  ["SuperCollider:out_2"  "AG06/AG03 Analog Stereo:playback_FR"]])

(jack/connections)

(reset! overtone.sc.machinery.server.comms/osc-debug* true)

(macroexpand-1
 '
 (defsynth foo [freq 440]
   (out 0 (+ (sin-osc freq)
             (* 0.5 (sin-osc (* 2 freq)))
             (* 0.33 (sin-osc (* 3 freq)))
             (* 0.25 (sin-osc (* 4 freq)))))))

(synth foo [freq {:default 440}]
       (out 0 (+ (sin-osc freq)
                 (* 0.5 (sin-osc (* 2 freq)))
                 (* 0.33 (sin-osc (* 3 freq)))
                 (* 0.25 (sin-osc (* 4 freq))))))

(macroexpand-1
 '
 (synth foo [freq 440]
        (out 0 (+ (sin-osc freq)
                  (* 0.5 (sin-osc (* 2 freq)))
                  (* 0.33 (sin-osc (* 3 freq)))
                  (* 0.25 (sin-osc (* 4 freq)))))))
(:sdef foo)

(overtone.sc.machinery.synthdef/synthdef-decompile (:sdef foo))
(definst foo [t {:rate :kr :default 440}]
  (sin-osc t))
(inst foo [t {:rate :kr :default 440}]
      (sin-osc t))

(synth foo [freq 440]
       (out 0 (+ (sin-osc freq)
                 (* 0.5 (sin-osc (* 2 freq)))
                 (* 0.33 (sin-osc (* 3 freq)))
                 (* 0.25 (sin-osc (* 4 freq))))))

(demo (example dibrown :rand-walk))
