(ns mad-sounds.sessions.aarschot-2026-02-01-fm-pm
  (:require
   [overtone.live :refer :all]
   [vibeflow.session :as session]
   [vibeflow.util :as util]
   [casa.squid.jack :as jack]
   [casa.squid.midi :as midi]))

(defonce jack (jack/client :vibeflow))
(defonce midi-out (jack/direct-midi-out-port jack :out))
(defonce midi-in (jack/midi-in-port jack :in))

(midi/write midi-out (midi/message 0 :cc 12 100))
( jack midi-in println)


(defn midi-ctl [synth-key synth mapping]
  (let [params     (:params (if (var? synth) @synth synth))
        midi-state (atom {})
        mapping    (into {} (map (juxt val key)) mapping)
        mapping    (into {} (keep (fn [param]
                                    (when-let [c (get mapping (keyword (:name param)))]
                                      [c param])))
                         params)]
    (prn mapping)
    (doseq [[cc {:keys [min max value]}] mapping]
      (midi/write midi-out (midi/message 0 :cc cc (* 127 (/ (- @value min) (- max min))))))
    (jack/set-midi-callback!
     jack midi-in
     (fn [msg _]
       (let [[ch type data1 data2] (midi/event msg)]
         (when (= type :cc)
           (println :cc data1 data2)
           (when-let [{:keys [name default min max step value]
                       :or {min 0}}
                      (get mapping data1)]
             (let [synth (if (var? synth) @synth synth)
                   v (if (and min max) (/ data2 127) data2)
                   v (if (and min max) (+ min (* (- max min) v)) v) ;; 0-127 -> min-max
                   v (if step
                       (* step (Math/round (double (/ v step))))
                       v)] ;; round to nearest step
               (println name "=" (double v))
               (reset! (:value (first (filter (comp #{name} :name) (:params synth)))) v)
               (ctl synth (keyword name) v)))))))))

(definst dx [freq 130.80
             amp 1.0
             gate 1.0

             l1 {:min 0 :max 1 :default 0.8}
             l2 {:min 0 :max 2 :default 0}
             m1 {:min 0 :max 10 :default 1 :step 1}
             m2 {:min 0 :max 10 :default 1 :step 1}

             fb2 {:min 0 :max 10 :default 0}

             a1 {:doc "op1 attack rate" :min 0 :max 1.0 :default 0.1}
             d1 {:doc "op1 decay rate" :min 0 :max 3.0 :default 0.2}
             s1 {:doc "op1 sustain level" :min 0 :max 1.0 :default 0.75}
             r1 {:doc "op1 release rate" :min 0 :max 8.0 :default 0.5}

             a2 {:doc "op1 attack rate" :min 0 :max 1.0 :default 0.1}
             d2 {:doc "op1 decay rate" :min 0 :max 3.0 :default 0.2}
             s2 {:doc "op1 sustain level" :min 0 :max 1.0 :default 0.75}
             r2 {:doc "op1 release rate" :min 0 :max 8.0 :default 0.5}

             pm-scale 4.0
             ]
  (let [op2
        (* l2
           (env-gen (adsr a2 d2 s2 r2) gate)
           (* (+ m2 (* 0.5 (< m2 1))) freq)
           (sin-osc-fb freq (* fb2 (/ Math/PI 7.0))))
        op1
        (* l1
           (env-gen (adsr a1 d1 s1 r1) gate :action FREE)
           (sin-osc (* (+ m1 (* 0.5 (< m1 1))) freq) (* pm-scale op2)))]
    op1))

(db->amp 70)

(midi-ctl :dx dx {2 :l1
                  3 :m1
                  4 :a1
                  5 :d1
                  6 :s1
                  7 :r1

                  8 :l2
                  9 :m2
                  10 :fb2
                  12 :a2
                  13 :d2
                  14 :s2
                  15 :r2
                  })

(:params dx)

(dx)
(stop)

(util/keyboard-insts dx)

control-proxy-cache
