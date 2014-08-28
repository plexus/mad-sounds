(ns mad-sounds.bay-beat
  (:require [overtone.live :refer :all]
            [overtone.inst.synth :refer :all]
            [mad-sounds.inst.sampled :refer :all]
            [mad-sounds.inst.synths :refer :all]))

(def *base* (note :C3))
(def *scale* (partial nth-interval :major))

(defn scale-nth [idx]
  (+ *base* (*scale* idx)))

(def *sig* 8)
(def drums [[kick-fat  [0 2.5 4 6.5]]
            [snare-fat [1 3 5 7]]
            [click-s   [1/2 3 7/2 5 6 6.75 7]]])


(map vector [:a :b :c] (range))
;; [[:a 0] [:b 1] [:c 2]]


(def ^:dynamic *bass-line* (map scale-nth [0 2 0 2  6 0 5 4  0 0 0 2  0 5 -1 3]))

(def *m* (metronome 90))
(*m*)

(let [m (metronome 120)]
  (m)
  (m 0) ;; 95898254s 450ms
  (m 1) ;; 95898254s 550ms
  (m 2) ;; 95898254s 650ms
  ((from m 2) 0)
  (let [new-metronome (from m 2)]
    (new-metronome 0) ;; 95898254s 650ms
    )
  )

(defn from [metro offset]
  (fn [beat]
    (metro (+ beat offset))))

(defn even-melody [timer inst [note & notes]]
  (do
    (at (timer 0) (let [i (inst note)]
                      (at (timer 1) (ctl i :gate 0))))
    (let [next (from timer 0.5)]
      (if notes
        (even-melody next inst notes)
        next))))

(defn loop-bass [timer inst]
  (let [next (even-melody timer inst *bass-line*)]
    (apply-by (next 0) #'loop-bass next inst [])))


(defn play! [m]
  (loop-bass m vintage-bass)
  (loop-beat m))

(play! (metronome 90))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment


(defn -rewrite-ats [timervar]
  (fn [forms]
    (for [form forms]
      (if (list? form)
        ((-rewrite-ats timervar)
         (if (= (first form) 'at)
           (seq (assoc-in (vec form) [1] `(~timervar ~(nth form 1))))
           form))
        form))))

(defmacro defloop [name count args & body]
  (let [timervar 'timer#
        newbody (map (-rewrite-ats timervar) body)]
    `(defn ~name [~timervar ~@args]
       ~@newbody
       (apply-by (~timervar ~next) #'~name [(from ~timervar ~count) ~@args]))))


  (let [m (metronome 120)]
    (loop-bass m #(cs80lead (midi->hz %))))

  (let [m (metronome 120)]
    (loop-bass m #(tb303 (midi->hz %) :amp 1)))


  (defn from [metro offset]
    (fn [beat] (metro (+ beat offset))))

  (defn kick-and-snare [metro]
    (at (metro 1) (kick-fat))
    (at (metro 2) (snare-fat))
    (apply-by (metro 3) #'kick-and-snare [(from metro 2)]))



  (def *m* (metronome 120))

  (*m* 1)

  (loop-beat (from *m* (*m*)))
  (loop-bass (from *m* (*m*)) vintage-bass)

  (loop-bass (metronome 120) vintage-bass)

  (stop)

  (resolve-scale :ionian)
  (choose (scale-field :g :ionian))

  (stop)
  (stop)
  (kick-fat)
  (snare-fat)

  (click-s)
  (boom-s)
  (flute-A)
  (volume 0.3))
