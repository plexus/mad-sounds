(ns mad-sounds.sessions.aarschot-2024-01-12-compilation
  (:use overtone.live
        overtone.sc.machinery.synthdef
        vibeflow.util))

(spectro)

(synthdef-read "./target/tick.scsyndef"#_"./target/test.scsyndef" #_"target/sinosc2.scsyndef")

(demo (pan2 (mix (* (sin-osc (for [i (range 10)]
                               (* (inc i) 300)))
                    (for [i (range 10)]
                      (if (even? i)
                        (- 1 (* i 0.1))
                        0.1))))))
definst
(defsynth mysin [freq 440 mul 0.5]
  (out 0 (sin-osc freq 0.0 mul)))

(def m (mysin))
(stop)

(synthdef-decompile
 (:sdef mysin))

[
 (synthdef-read "target/sinosc3.scsyndef")
 (synthdef-read (synthdef-bytes (:sdef mysin)))]
