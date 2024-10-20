(ns vibeflow.freesound
  (:require [overtone.live :refer :all]))

(def sample-ids
  {:boom-s     33637
   :clap       48310
   ;; :clap2       132676
   :click-s    406
   :count-down 71128
   :cowbell    9780
   :cymbal     13254
   ;; :dirty-kick  30669
   :flute      35809
   :flute-A    107307
   :kick       16699 ;; pretty natural acoustic kick
   :kick-d     41155
   :kick-fat   2086 ;; dance style kick
   :kick-thin  777
   :kick-prog  385874
   :kicky      512175
   :ride       436
   :snap       87731
   :subby      25649
   :tom        184536
   :tom2       47700
   :wop        85291

   :snare         216045
   :snare2        26903
   :snare-fat     122053
   :snare-bright  594282

   :hihat-click   183401
   :hihat-closed  802
   :hihat-closed2 506453
   :hihat-open    183105
   :hihat-open2   26657
   :ring-hat      12912
   :zg-hat        72526

   :stick 628568})

(defonce samples (atom {}))

(def fbuf* (memoize freesound))

(defn fbuf [k] (fbuf* (get sample-ids k)))

(defmacro fsample [sname]
  (let [sid (get sample-ids sname)]
    `(or (get @samples ~sid)
         (let [{n-ch# :n-channels
                id# :id} (fbuf* ~sid)
               inst# (inst ~(name sname) [~'amp 1]
                           (~'* ~'amp
                            (scaled-play-buf n-ch# id# :action FREE)
                            :force-ugen))]
           (swap! samples assoc ~sid inst#)
           inst#))))

(run! freesound (vals sample-ids))

;; ((fsample :kick))
;; ((fsample :kick-d))
;; ((fsample :kick-fat))
;; ((fsample :kick-thin))
;; ((fsample :kick-prog))
;; ((fsample :kicky))
;; :kick-d
;; :kick-fat
;; :kick-thin
