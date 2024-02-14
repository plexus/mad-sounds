(ns mad-sounds.sessions.aarschot-2024-01-21-snowday
  (:require
   [overtone.live :refer :all]
   [vibeflow.freesound :refer [fsample fbuf]]
   [vibeflow.util :as util :refer [defloop]]
   [vibeflow.controller-ui :as cui]
   #_:reload))

(def ride (fsample :ride))

(definst bass [note 40
               gate 1]
  (let [freq (midicps note)]
    (pan2 (* (env-gen (adsr) :gate gate :action FREE)
             (+ (* 0.4 (sin-osc freq))
                (moog-ff
                 :gain 2
                 :freq (* 6.5 freq)
                 :in (pluck :in (brown-noise)
                            :delaytime (/ 1 freq))))))))

(defloop swing-ride 2 [m b]
  (at (m (+ b 0)) (ride :amp (+ (/ (rand) 10) 0.8)))
  (at (m (+ b 1)) (ride :amp (+ (/ (rand) 10) 1)))
  (at (m (+ b 5/3)) (ride :amp  (+ (/ (rand) 10) 0.8)))
  ;; (at (m (+ b 7/4)) (ride :amp  (+ (/ (rand) 10) 0.6)))
  )

(def progression [[:c2 :m7]
                  [:f2 :dom7]
                  [:bb2 :major7]
                  [:eb2 :major7]
                  [:a1 :m7-5]
                  [:d2 :dom7]
                  [:g2 :m6]
                  [:g2 :m6]])

(defloop bass-line 4 [m b]
  (let [[root qual] (get progression (mod b 8))
        [i iii v vii] (chord root qual)]
    (let [bi (at (m (+ b 0)) (bass i))
          bi (at (m (+ b 1)) (ctl bi :gate 0) (bass v))
          bi (at (m (+ b 2)) (ctl bi :gate 0) (bass iii))
          bi (at (m (+ b 3)) (ctl bi :gate 0) (bass i))]
      (at (m (+ b 4)) (ctl bi :gate 0)))
    ))

(bass)
(stop)
(kill bass-line)
(kill-player #'bass-line)

(map #(apply chord %) progression)

(inst-volume! bass 1)

(let [m  (metronome 120)]
  (swing-ride m)
  (bass-line m))
(stop)

(kill bass-line)

(:params ride )

(swing-ride (metronome 120))
