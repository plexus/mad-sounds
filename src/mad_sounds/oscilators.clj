(ns mad-sounds.oscilators
  (:use overtone.live))

(comment

  (definst sin-wave [freq 440 attack 0.01 sustain 0.4 release 0.1 vol 0.4]
    (* (env-gen (lin-env attack sustain release) 1 1 0 1 FREE)
       (sin-osc freq)
       vol))

  (definst saw-wave [freq 440 attack 0.01 sustain 0.4 release 0.1 vol 0.4]
    (* (env-gen (lin-env attack sustain release) 1 1 0 1 FREE)
       (saw freq)
       vol))

  (definst square-wave [freq 440 attack 0.01 sustain 0.4 release 0.1 vol 0.4]
    (* (env-gen (lin-env attack sustain release) 1 1 0 1 FREE)
       (lf-pulse freq)
       vol))

  (definst noisey [freq 440 attack 0.01 sustain 0.4 release 0.1 vol 0.4]
    (* (env-gen (lin-env attack sustain release) 1 1 0 1 FREE)
       (pink-noise) ; also have (white-noise) and others...
       vol))

  (definst triangle-wave [freq 440 attack 0.01 sustain 0.1 release 0.9 vol 0.4]
    (* (env-gen (lin-env attack sustain release) 1 1 0 1 FREE)
       (lf-tri freq)
       vol))

  (triangle-wave))
