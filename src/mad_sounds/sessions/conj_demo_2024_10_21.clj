(ns mad-sounds.sessions.conj-demo-2024-10-21
  (:require
   [overtone.live :refer :all]
   [overtone.studio.transport :as transport]
   [vibeflow.freesound :as f]
   [vibeflow.studio :as ui]
   [vibeflow.synths :as s]))



(demo (pan2 (sin-osc)))
(demo (pan2 (var-saw)))
(demo (pan2 (pulse :width (line 0.1 0.5 2))))
(demo (pan2 (square)))
(demo (pan2 (var-saw :width (line 0 0.5 2))))
(demo (pan2
       (let [freq       440
             trig       (var-saw:kr 25)
             freq-slide (-> (line:kr freq (/ freq 2) 2)
                            (latch trig))
             sig        (pluck (white-noise)
                               trig
                               :delaytime (/ 1 freq-slide)
                               :decaytime 30)]
         (rlpf
          sig
          (* (lin-lin (sin-osc (/ freq-slide 4)) -1 1 0.8 1.2) freq-slide 5)
          1/8))))


(demo (pan2 (var-saw :width (line 0 0.5 2))))

(demo (pan2 (* 2 (env-gen (perc :release 0.5))
               (rlpf (white-noise) 440 0.001))))

(doseq [n [:c4 :g4 :d5 :a5]]
  (let [freq (midi->hz (:midi-note (note-info n)))]
    (demo (pan2
           (rlpf
            (* 2
               (env-gen (perc))
               (+ (* 1/2 (rlpf (pink-noise) freq 0.001))
                  (* 3/4 (pulse freq :width 0.5))))
            (* 6 freq)
            ))))
  (Thread/sleep 500))


(doseq [n [60 62 64]]
  (demo (pan2 (let [f (midi->hz n)
                    env (env-gen (perc))]
                (* env
                   (rlpf
                    (+ (var-saw f)
                       (var-saw (* 3 f))
                       (var-saw (* 9.05 f)))
                    (lin-lin env 0 1 (* 3 f) (* 9 f))
                    1/2)))))
  (Thread/sleep 200))
