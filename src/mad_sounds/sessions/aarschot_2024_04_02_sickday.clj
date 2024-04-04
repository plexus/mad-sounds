(ns mad-sounds.sessions.aarschot-2024-04-02-sickday
  (:require
   [mad-sounds.euclid :refer :all]
   [overtone.core :refer :all]
   [overtone.studio.transport :as transport]))


(connect-server 55555)
(overtone.sc.machinery.server.connection/connect-jack-ports)

(require '[overtone.inst.drum :as drum]
         '[overtone.inst.synth :as synth]
         '[overtone.synth.stringed :as stringed]
         '[overtone.synth.ixi :as ixi]
         '[overtone.synth.retro :as retro]
         '[vibeflow.freesound :as fs])

(pplay :hat
       (pbind {:instrument (fs/fsample :hihat-closed)
               :swing 1/8
               :dur [0.5]
               :amp (pdo [(rrand 0.5 0.8)
                          (rrand 0.3 0.6)])
               :swing-quant 1}
              ))

(definst fm [freq 440
             ratio 3
             depth 0.5
             gate 1
             amp 1]
  (let [mod (sin-osc (* freq ratio))
        car (sin-osc freq :phase (* mod depth))
        env (env-gen (adsr) :gate gate :action FREE)]
    (* (moog-ff car (* freq 10)) env amp)))

(pplay :melody
       (pbind {:instrument fm
               :degree (pdo
                         (concat
                          [[0 :rest 4 :rest 0 :rest]]
                          (shuffle [[5 2 0 :rest :rest :rest]
                                    [0 2 4 5 :rest :rest]
                                    [0 1 :rest 6 7 :rest]
                                    [-8 -1 0 :rest :rest :rest]])
                          [[:rest :rest :rest :rest :rest :rest]]))
               :swing 1/8
               :ratio 3
               :depth 1
               :amp (pwhite 0.1 0.2)
               :dur [0.5]
               :octave [4 5]
               :scale :major-pentatonic}))

(stop)
