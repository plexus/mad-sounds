(ns mad-sounds.sessions.aarschot-2023-12-29-vakantie-take2
  (:require
   [overtone.live :refer :all]
   [vibeflow.freesound :refer [fsample fbuf]]
   [vibeflow.util :as util :refer [defloop]]
   [vibeflow.controller-ui :as cui]
   #_:reload))

(defn activate-inst [inst]
  (cui/ctl-synth! inst)
  (util/midi-ctl :keys inst)
  (util/midi-preview 0 :keys inst))

(definst whomp [note {:default 60 :max 85}
                cutoff {:min 0 :default 55 :max 100}
                mod-freq {:min 0 :default 1.2 :max 6}
                mod-depth {:min 0 :default 0.5 :max 1}
                release {:min 0 :default 0.1 :max 1}
                resonance {:min 0 :default 14 :max 100}
                phase {:min 0 :default 0 :max 2}
                gate 1]
  (let [freq (midicps note)
        freq2 (+ freq (* freq (lf-tri mod-freq :iphase phase) mod-depth))]
    (* (env-gen (asr :release release) :gate gate :action FREE)
       (rlpf
        (square freq2)
        (* freq2 (+ 1 (* cutoff 3/100)))
        :rq (/ 1 (+ 1/10000 resonance))))))

(comment
  (activate-inst #'whomp))

(definst pad [note {:default 60 :max 85}
              release {:min 0 :default 0.5 :max 1}
              mod-depth {:min 0 :default 0.71 :max 10}
              mod-freq {:min 0 :default 7.48 :max 10}
              gate 1]
  (let [freq (midicps note)
        mod (sin-osc (* (+ 1 (* mod-depth (sin-osc (* freq (/ 1 (- 10 mod-freq)))))) freq))]
    (* (env-gen (asr :release release) :gate gate :action FREE)
       (rlpf
        (sin-osc freq :phase mod)
        :freq (* 1.5 freq)))))

(comment
  (activate-inst #'pad))

(defloop hats 1 [m b]
  (let [s (fsample :hihat-closed2)]
    (at (m (+ 0   b)) (s :amp (+ 0.8 (rand))))
    (at (m (+ 0.5 b)) (s :amp (+ 0.7 (rand))))))

(defloop kicks 4 [m b]
  (let [s (fsample :kicky)]
    (at (m (+ 0   b)) (s :amp (+ 1)))
    (if (< 0.5 (rand))
      (at (m (+ 0.5   b)) (s :amp (+ 1))))
    ;; (at (m (+ 1.75 b)) (s :amp (+ 0.8 (rand))))
    ;; (at (m (+ 2  b)) (s :amp (+ 0.8 (rand))))
    (if (< 0.5 (rand))
      (at (m (+ 2.25 b)) (s :amp (+ 0.8 (rand))))
      (at (m (+ 2.5  b)) (s :amp (+ 0.8 (rand)))))
    ))

(defloop snares 4 [m b]
  (let [s (fsample :snare-bright)]
    (at (m (+ 1 b)) (s :amp (+ 1)))
    (at (m (+ 3 b)) (s :amp (+ 1)))
    ;; (at (m (+ 3.5 b)) (s :amp (+ 1)))
    ))

(defloop whomper 16 [m b]
  (let [b #(m (+ %1 (* %2 1/4 ) b))]
    (at (b 0 0) (whomp :note 40))
    (at (b 0 2) (whomp :note 47))
    (at (b 1 0) (whomp :note 44))
    (at (b 1 3) (ctl whomp :gate 0))

    (at (b 4 0) (whomp :note 49))
    (at (b 4 2) (whomp :note 56))
    (at (b 5 0) (whomp :note 52))
    (at (b 5 2) (ctl whomp :gate 0))

    (at (b 8 0) (whomp :note 47))
    (at (b 8 2) (whomp :note 51))
    (at (b 9 0) (ctl whomp :gate 0))

    (at (b 12 0) (whomp :note 45))
    (at (b 13 0) (ctl whomp :gate 0))
    ))

(def get-sample
  (memoize (fn [s]
             (sample (str "samples/" (name s) ".wav")))))

(comment
  ((get-sample :bring-it-on))
  ((get-sample :dont-say-a-word))
  ((get-sample :spring-it-on-me))
  ((get-sample :do-do-den-do-dee)))

(definst play-vocal [buf (get-sample :bring-it-on)]
  (pan2
   (pitch-shift
    (scaled-play-buf 1 buf :rate 164/120 :action FREE)
    :window-size 0.4
    :pitch-ratio 120/164)


   #_
   (free-verb

    :mix 0.2
    :room 0.1
    :damp 0.2)))

(def m (metronome 164))

(defn go! []
  (metro-start m 0)
  (hats m)
  (kicks m)
  (snares m)
  #_(whomper m))

;; two round buttons on novation launchkey next to the pads
(on-event [:midi :control-change] (fn [{:keys [data1 data2]}]
                                    (case [data1 data2]
                                      [108 127] (go!)
                                      [109 0] (stop)
                                      nil))
          ::stop)

(cui/show!)

(util/midi-pad 9 1 :bring-it-on #(play-vocal (get-sample :bring-it-on)))
(util/midi-pad 9 2 :spring-it-on-me #(play-vocal (get-sample :spring-it-on-me)))
(util/midi-pad 9 3 :dont-say-a-word #(play-vocal (get-sample :dont-say-a-word)))
(util/midi-pad 9 4 :do-do-den-do-dee #(play-vocal (get-sample :do-do-den-do-dee)))

(def grains (overtone.sc.buffer/buffer-alloc-read-channel #_"samples/do-do-den-do-dee.wav"
                                                          "/home/arne/Overtone/Edward_Mika-Walc_Porzucona_Abandoned Waltz-1929.wav"
                                                          #_"/home/arne/Overtone/mad-sounds/xylophone.mp3" 0))

(definst grainer [note {:default 60 :max 90}
                  trigf {:default 400 :max 1000}
                  grain-size {:default 0.1 :max 0.2}
                  start {:default 0.1 :max 1}
                  length {:default 0.2 :max 1}
                  pos-freq {:default 1 :max 4}
                  rate {:default 1 :max 127}
                  gate 1]
  (let [freq (midicps note)]
    (* (env-gen (asr :release 0.05) :gate gate :action FREE)
       (grain-buf 1
                  :trigger (impulse trigf)
                  :dur grain-size
                  :sndbuf (:id grains)
                  :rate (* rate (/ freq 400))
                  :pos (+ start (* length (var-saw:kr pos-freq)))
                  :interp 2))))

(activate-inst #'grainer)

(definst grainer2 [trigf {:default 400 :max 2000}
                   note {:default 60 :max 90}
                   rate {:default 1 :max 3}
                   grain-size {:default 0.1 :max 0.2}
                   amp {:default 0.1 :max 1}
                   start {:default 0.1 :max 1}
                   length {:default 0.2 :max 1}
                   pos-freq {:default 1 :max 127}
                   gate 1]
  (let [freq (midicps note)
        buf  (:id grains)]
    (* (env-gen (asr :attack 0.1 :release 0.05) :gate gate :action FREE)
       (t-grains 1
                 :trigger (impulse trigf)
                 :bufnum buf
                 :rate (* rate (/ freq 400))
                 :center-pos (* (+ start (/ (* length (var-saw:kr pos-freq))
                                            (buf-dur buf)))
                                (buf-dur buf))
                 :dur grain-size
                 :amp amp))))

(:params grainer2)
(stop)

(demo 3 (pan2 (sin-osc (midi->hz 60))))

(defn ctl* [synth & kvs]
  (apply ctl synth kvs)
  (let [m (apply hash-map kvs)]
    (doseq [{pname :name pval :value} (:params synth)
            :let [v (get m (keyword pname))]
            :when v]
      (reset! pval v))))

(util/capture-ctls #'grainer2)
(ctl* grainer2 :trigf 16000/127 :rate 69/127 :grain-size 0.09763779527559056 :amp 124/127 :start 22/127 :length 3/127 :pos-freq 9N)
(ctl* grainer2 :trigf 20000/127 :rate 144/127 :grain-size 0.07244094488188976 :amp 123/127 :start 31/127 :length 95/127 :pos-freq 2600/127)

(activate-inst #'grainer2)

(demo
  (pan2
   (t-grains 1
             :trigger (impulse (midi->hz 55))
             :bufnum (:id grains)
             :rate 1 #_(/ freq 400)
             :center-pos 0.2 #_(* pos (buf-dur:ir buf))
             :dur 0.1
             :amp 0.2)))

;;;;;;;;;;;;;;;;;;;;
(comment
  (clojure.repl/apropos "warp")
  (stop)
  (event-debug-on)
  (event-debug-off)
  (osc-debug true)
  (osc-debug false)
  (m :bpm 173)


  (def wt (buffer (Math/pow 2 15)))

  (buffer-write! wt (signal->wavetable (buffer-read bring-it-on-sample 20000 (long (Math/pow 2 14)))))





  (get-sample :dont-say-a-word)

  (sample )

  c-osc
  v-osc
  v-osc3
  shaper
  osc-n

  (grain-buf)
  (t-grains)
  (warp1)
  grain-fm
  grain-in
  p-sin-grain
  t-grains2
  t-grains3
  lf-noise0
  lf-noise1
  lf-noise2
  lfd-noise3
  dust
  dust2
  impulse
  )
