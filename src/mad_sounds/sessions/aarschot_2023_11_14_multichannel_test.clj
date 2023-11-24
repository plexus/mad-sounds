(ns mad-sounds.sessions.aarschot-2023-11-14-multichannel-test
  (:use overtone.live))


;; I assumed this would work but it breaks

(defsynth control-proxy-usable [freq 440 num-channels 8]
  (let [sig (sin-osc:ar freq)
        pan-control (sin-osc:kr 0.2)
        nc (:value num-channels)]
    (out (range nc)
         (* sig (max 0 (sin (* 0.5 Math/PI (+ pan-control (- (* 2 (map #(/ % nc) (range nc))) 1.5)))))))))

(control-proxy-usable)

;; Execution error (ClassCastException) at den1k.surround/fn$fn (REPL:188).
;; class overtone.sc.machinery.ugen.sc_ugen.ControlProxy cannot be cast to class java.lang.Number (overtone.sc.machinery.ugen.sc_ugen.ControlProxy is in unnamed module of loader clojure.lang.DynamicClassLoader @1d150b18; java.lang.Number is in module java.base of loader 'bootstrap')

;; this works but is more verbose

(defsynth control-proxy-not-usable-directly [freq 440 num-channels 8]
  (let [sig (sin-osc:ar freq)
        pan-control (sin-osc:kr 0.2)
        nc (:value num-channels)]
    (for [ch (range nc)]
      (out ch  (* sig (max 0 (sin (* 0.5 Math/PI (+ pan-control (- (* 2 (/ ch nc)) 1.5))))))))))
