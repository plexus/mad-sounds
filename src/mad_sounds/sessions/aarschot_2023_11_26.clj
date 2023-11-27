(ns mad-sounds.sessions.aarschot-2023-11-26
  (:require
   [casa.squid.jack :as jack]
   [overtone.live :refer :all]))

(jack/connect!
 (map vector
      (filter #(re-find #"Overtone|SuperCollider" %)
              (jack/ports @jack/default-client #{:audio :out}))
      (jack/ports @jack/default-client #{:audio :physical :in})))
(do #'now)

(definst g [knum 12]
  (gendy3 :knum knum))

(kill g)

(ctl g :knum 6)

(demo (var-saw :width 0.8))
(demo 2 (-> (formant :fundfreq 300 :formfreq

                     (* (vibrato :freq 3 :rate 40)
                        (+ 600 (* 3000 (abs (lf-saw:kr 0.2))))))
            (rlpf :freq 3000)))
resonz one-pole one-zero two-pole two-zero apf
integrator decay decay2 lag lag2 lag3 ramp lag-ud
lag2-ud lag3-ud leak-dc rlpf rhpf hpf bpf brf
mid-eq lpz1 lpz2 hpz1 hpz2 slope bpz2 median slew
sos ringz formlet detect-silence

osc sin-osc sin-osc-fb osc-n v-osc v-osc3 c-osc
formant lf-saw lf-par lf-cub lf-tri lf-gauss
lf-pulse var-saw impulse sync-saw wrap-index
index-in-between detect-index shaper degree-to-key
select vibrato index
line x-line lin-exp lin-lin amp-comp amp-comp-a k2a
a2k t2k t2a dc silent
