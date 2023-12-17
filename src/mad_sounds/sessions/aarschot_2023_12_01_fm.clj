(ns mad-sounds.sessions.aarschot-2023-12-01-fm
  (:require
   [casa.squid.jack :as jack]
   [overtone.at-at :as at]
   [overtone.live :refer :all]
   [overtone.studio.midi :as midi]
   #_[overtone.gui.control :as gui-control]
   [vibeflow.util :as util]))

(jack/connect! (util/overtone-conns))

(definst fm-stuff [note {:default 60 :min 0 :max 120 :step 1}
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

(midi-preview :fm fm-stuff)

(defn midi-preview [synth-key synth]
  (let [voices (atom {})]
    (on-event [:midi :note-on]
              (fn [{:keys [note]}]
                (swap! voices assoc note (synth :note note)))
              [synth-key :on])
    (on-event [:midi :note-off]
              (fn [{:keys [note]}]
                (ctl (get @voices note) :gate 0)
                (swap! voices dissoc note))
              [synth-key :off]))
  (let [midi-state (atom {})
        mapping    (into {} (map-indexed (fn [idx p]
                                           [
                                            (+ 21 idx) p])) (:params synth))]
    (prn mapping)
    (on-event [:midi :control-change]
              (fn [{:keys [data1 data2]}]
                (when-let [{:keys [name default min max step value]}
                           (get mapping data1)]
                  (prn name )
                  (let [v (/ data2 127)
                        v (+ min (* (- max min) v)) ;; 0-127 -> min-max
                        v (* step (Math/round (double (/ v step))))] ;; round to nearest step
                    (reset! value v)
                    (ctl synth (keyword name) v)
                    ))
                )
              [synth-key :ctl])))

(midi->hz 69)

(def at-pool (at/mk-pool))
(at/every 1000 #'fm-stuff at-pool)
(run! at/kill (at/scheduled-jobs at-pool))
(fm-stuff)
(stop)

(doseq [{:keys [name value]} (:params fm-stuff)]
  (add-watch value ::ui->ctl
             (fn [k r o n]
               (ctl fm-stuff (keyword name) n))))

(ctl fm-stuff :mod-interval 12)

(gui-control/synth-controller fm-stuff)
