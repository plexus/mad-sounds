(ns mad-sounds.sessions.aarschot-2024-08-31-summers-gone
  (:require
   [casa.squid.plasticine :as p]
   [overtone.live :refer :all]
   [quil.core :as q]))

(defn param [inst name]
  (some #(when (= name (:name %)) %)
        (:params inst)))

(definst bass [freq 70
               brightness {:min 0 :default 1.1 :max 4}
               attack {:min 0 :default 0.03 :max 0.2}
               decay {:min 0 :default 0.14 :max 0.8}
               sustain {:min 0 :default 0.61 :max 1.5}
               release {:min 0 :default 0.47 :max 0.5}
               amp  {:min 0 :default 1 :max 1.5}
               gate 1]
  (let [env (env-gen (adsr attack decay sustain release)
                     :gate gate
                     :action FREE)]
    (-> (var-saw freq :width 0)
        (rlpf (* brightness freq env))
        (* amp env))))

(definst moog-bass [freq 110
                    attack      {:min 0 :default 0.01 :max 0.1}
                    decay       {:min 0 :default 0.01 :max 0.3}
                    sustain     {:min 0 :default 0.82 :max 1.5}
                    release     {:min 0 :default 0.24 :max 0.5}
                    amp         {:min 0 :default 1.60 :max 2.5}
                    brightness  {:min 0 :default 3.03 :max 12}
                    osc1-amp    {:min 0 :default 2.53 :max 6}
                    osc2-amp    {:min 0 :default 4.14 :max 6}
                    osc3-amp    {:min 0 :default 1.75 :max 6}
                    lfo-freq    {:min 0 :default 3.16 :max 10}
                    shape       {:min 0 :default 0.07 :max 1}
                    noise       {:min 0 :default 0.57 :max 5}
                    shape-depth {:min 0 :default 0.17 :max 0.25}
                    gate 1]
  (let [env (env-gen (adsr attack decay sustain release) :gate gate :action FREE)]
    (pan2
     (*
      amp
      (mix (* [osc1-amp (* env osc2-amp) (* env osc3-amp)]
              [(moog-ff
                :gain 2
                :freq (* 3.4 freq)
                :in (* 4 (pluck :in (brown-noise)
                                :delaytime (/ 1 freq))))
               (sin-osc freq)
               (rlpf
                (drive-noise
                 (var-saw freq :width (* (lin-lin (sin-osc lfo-freq)
                                                  0 1
                                                  1 (+ 1 shape-depth))
                                         shape))
                 noise)
                (* env brightness freq))]))))))

(definst muckleflute [freq 440
                      attack      {:min 0 :default 0.86 :max 1}
                      decay       {:min 0 :default 0.00 :max 1}
                      sustain     {:min 0 :default 0.99 :max 1.5}
                      release     {:min 0 :default 0.9 :max 1}
                      gate 1
                      amp 1]
  (splay
   (* (env-gen (adsr attack decay sustain release) :gate gate :action FREE)
      amp
      (rlpf (square (freq-spread freq 4))
            (* 5 freq)
            0.1))
   :spread (env-gen (envelope [1 0 1] [0.05 0.05]))
   :level-comp? false))

(definst brock [freq 220
                gate 1]
  (* (env-gen (asr :attack 0.01 :release 0.1) :gate gate :action FREE)
     (lpf
      (pluck (brown-noise) :delaytime (/ 1 freq) :decaytime 2)
      (* 3 freq))))

(definst rusty [freq 300
                amp 0.3
                gate 1]
  (* (env-gen (perc) :gate gate :action FREE)
     amp
     (moog-ff
      (+ (* 0.2 (var-saw :width 1 :freq (repeatedly 8 #(+ freq (midiratio (srand 0.1))))))
         (square freq))
      (* 30 freq))))

(defn keyboard-insts [& insts]
  (on-event [:midi :note-on]
            (fn [{:keys [note channel velocity] :as e}]
              (when-let [inst (get (vec insts) channel)]
                (event :note :instrument inst :midinote note
                       :overtone.studio.event/key note
                       :end-time nil
                       :amp (* 1.5 (/ velocity 128) @(:value (param inst "amp"))))))
            ::midi-on)

  (on-event [:midi :note-off]
            (fn [{:keys [note channel] :as e}]
              (when-let [inst (get (vec insts) channel)]
                (event :note-end :instrument inst :midinote note
                       :overtone.studio.event/key note
                       :end-time (now))))
            ::midi-off))

(defn vol-ctls [& insts]
  (on-event [:midi :control-change]
            (fn [{:keys [data1 data2-f] :as e}]
              (when (= 21 data1)
                (volume (* 1.5 data2-f))))
            ::vol-ctl)
  (doseq [[inst idx] (map vector insts (range))]
    (on-event [:midi :control-change]
              (fn [{:keys [data1 data2-f] :as e}]
                (let [vol (* 1.5 data2-f)]
                  (when (= (+ idx 22) data1)
                    (ctl inst :amp vol)
                    (reset! (:value (some #(when (= "amp" (:name %)) %)
                                          (:params inst)))
                            vol))))
              [::ctl idx])))

(vol-ctls moog-bass bass muckleflute rusty brock)
(keyboard-insts moog-bass bass muckleflute rusty brock)



(on-event [:midi :control-change]
          (fn [{:keys [data1 data2]}]
            (cond
              (= [108 0] [data1 data2])
              (run! presume (keys @pplayers))
              (= [109 0] [data1 data2])
              (run! ppause (keys @pplayers))))
          ::play-pause)

(let [inst muckleflute]
  (def sliders
    (for [{:keys [name min max value step]}
          (:params inst)
          :when (and min max)]
      (p/hslider {:min min
                  :max max
                  :step step
                  :model value
                  :on-change #(ctl inst name %)
                  :height 60
                  :format #(format "%s: %.2f" name (double %))})))
  (swap! app update :children
         (fn [children]
           (run! p/-cleanup children)
           sliders)))

(def app (p/stack sliders :margin 4 :gap 4))

(q/defsketch controllers
  :title       ""
  :settings    #(q/smooth 2) ;; Turn on anti-aliasing
  :features    [:resizable :keep-on-top]
  :middleware  [p/middleware]
  ::p/root     #'app
  :size        [323 200]
  ::p/defaults {:text-size     25
                :frame-rate    30
                :stroke        [0 0 0]
                :fill          [0 0 0]
                :stroke-weight 15
                :background    [235 214 125]
                :rect-mode     :corner
                :stroke-cap    :round})

(do
  (pplay ::bass
         (repeat
          (pbind
           {:instrument moog-bass
            :note (concat
                   [:f3 :f3  :f3  :a3  :a3]
                   [:f3 :f3  :f3  :a3  :a3]
                   [:f3 :f3  :f3  :a3  :a3]
                   [:f3 :f3  :f3  :a3  :a3]

                   [:bb3 :bb3  :f3  :a3  :a3]
                   [:bb3 :bb3  :f3  :a3  :a3]
                   [:bb3 :bb3  :f3  :a3  :a3]
                   [:bb3 :bb3  :bb3  :bb3  :a3])
            :dur [3/2   1  1/2  1/2  1/2]}))
         {:quant 16})

  (pplay ::flute
         (repeat (pbind
                  {:instrument muckleflute
                   :note [:c4 :f4 :c4 :g4 :c4 :bb4 :a4 :g4 :f4
                          :d4 :d5 :c6 :bb5 :a5 :d5 :c5 :_]
                   :dur  [3/2   5/2   3/2   5/2   3/2    3/2   1   4/2   2
                          3/2   3/2   1   3/2   3/2 1   2 6]
                   }
                  ))
         {:quant 16}))

(param moog-bass "amp")

(params-vec {:instrument moog-bass})
