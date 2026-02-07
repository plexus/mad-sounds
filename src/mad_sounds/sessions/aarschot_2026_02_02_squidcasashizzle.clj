(ns mad-sounds.sessions.aarschot-2026-02-02-squidcasashizzle
  (:require
   [overtone.live :refer :all]
   [vibeflow.session :as session]
   [vibeflow.util :as util]
   [casa.squid.jack :as jack]
   [casa.squid.midi :as midi]
   [casa.squid.midi.javax :as jxm]))

(defonce jack (jack/client :vibeflow))
(defonce midi-out (jack/midi-out-port jack :out))
(defonce midi-in (jack/midi-in-port jack :in))

(midi/add-receiver midi-in ::x (fn [msg offset]
                                 (println "Jack received" msg offset)))
(midi/remove-receiver midi-in ::x)

(defn midi-ctl [synth-key synth mapping]
  (let [params     (:params (if (var? synth) @synth synth))
        midi-state (atom {})
        mapping    (into {} (map (juxt val key)) mapping)
        mapping    (into {} (keep (fn [param]
                                    (when-let [c (get mapping (keyword (:name param)))]
                                      [c param])))
                         params)]
    (doseq [[cc {:keys [min max value]}] mapping]
      (midi/write midi-out [0 :cc cc (* 127 (/ (- @value min) (- max min)))]))
    (midi/add-receiver
     midi-in
     [::midi-ctl synth-key]
     (fn [msg _]
       (println "MIDICTL" msg)
       (let [[ch type data1 data2] msg]
         (when (= type :cc)
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
           (fb-sine-c freq (* fb2 (/ Math/PI 7.0))))
        op1
        (* l1
           (env-gen (adsr a1 d1 s1 r1) gate :action FREE)
           (sin-osc (* (+ m1 (* 0.5 (< m1 1))) freq) (* pm-scale op2)))]
    op1))

(midi-ctl :dx dx {1 :l1
                  2 :m1
                  5 :a1
                  6 :d1
                  7 :s1
                  8 :r1

                  9 :l2
                  10 :m2
                  11 :fb2
                  13 :a2
                  14 :d2
                  15 :s2
                  16 :r2
                  })

(dx)
(stop)



(util/keyboard-insts dx)

(comment
  (midi/write midi-out [0 :cc 1 10])

  (midi/add-receiver (jxm/find-input-device "Java")
                     ::x
                     (fn [msg offset]
                       (println "Java received" msg offset)))
  (midi/remove-receiver (jxm/find-input-device "Twister")
                        ::x)

  (event-debug-on)
  (map jxm/device-name (jxm/query-devices))

  (midi/write (jxm/find-output-device "Java")
              [0 :cc 1 127])

  (jxm/send
   (jxm/find-output-device "Java")
   (midi/message [0 :cc 1 0])
   0)

  (jxm/connect (jxm/find-input-device "Twister")
               (jxm/receiver-callback (fn [& args]
                                        (println '-> args))))

  (midi/write midi-out [0 :cc 1 10]))



#_(System/setProperty "overtone.studio.midi.device-filter" "java-jack")
