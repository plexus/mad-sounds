(ns mad-sounds.sessions.aarschot-2023-12-04-overtone-next
  (:require
   [casa.squid.jack :as jack]
   [overtone.live :refer :all]
   [vibeflow.util :as u]))

(jack/connect!
 (u/overtone-conns))

(use 'overtone.inst.synth)

(fm-demo)

(stop)

(set! *warn-on-reflection* true)

(definst bell [note {:default 60 :min 0 :max 120 :step 1}
               mod-interval {:default 0 :min -12 :max 24 :step 1}
               attack {:default 0.01 :min 0 :max 1 :step 0.01}
               decay  {:default 0.3 :min 0 :max 2 :step 0.01}
               sustain  {:default 1 :min 0 :max 2 :step 0.01}
               release {:default 0.5 :min 0 :max 2 :step 0.01}
               depth {:default 1 :min 1 :max 10 :step 0.1}
               gate {:default 1 :min 0 :max 1 :step 1}
               ]
  (let [freq    (midicps note)
        modfreq (midicps (+ note mod-interval))
        mod     (sin-osc :freq modfreq)]
    (* (env-gen (adsr attack decay sustain release)
                :gate gate
                :action FREE)
       (sin-osc :freq freq :phase (* mod depth)))))

(definst bell [note {:default 60 :min 0 :max 120 :step 1}
               mod-interval {:default 0 :min -12 :max 24 :step 1}
               attack {:default 0.01 :min 0 :max 1 :step 0.01}
               decay  {:default 0.5 :min 0 :max 2 :step 0.01}
               sustain  {:default 0 :min 0 :max 2 :step 0.01}
               release {:default 0.5 :min 0 :max 2 :step 0.01}
               depth {:default 1 :min 1 :max 10 :step 0.1}
               gate {:default 1 :min 0 :max 1 :step 1}
               ]
  (let [freq    (midicps note)
        modfreq (midicps (+ note mod-interval))
        mod     (pluck :in (white-noise) :delaytime (/ 1 modfreq))]
    (* (env-gen (adsr attack decay sustain release)
                :gate gate
                :action FREE)
       (sin-osc :freq freq :phase (* mod depth))
       (sin-osc :freq (* 1.002 freq)
                :phase (* (env-gen (adsr 0.01 0.3 1 0.1))
                          mod
                          depth
                          )))))

(u/midi-preview :bell #'bell)
(meta @#'sin-osc)
(boot-external-server)
