(ns mad-sounds.sessions.aarschot-2024-05-15
  (:require
   [overtone.live :refer :all]
   [overtone.inst.sampled-piano :refer :all]
   ))

(connect-jack-ports)
(connection-info)
(definst dean [freq 200
               gate 1
               amp 1]
  (splay
   (* (env-gen (adsr) :gate gate :action FREE)
      amp
      (rlpf
       (var-saw (freq-spread freq 4 0.4)
                :width (lin-lin freq 0 13000 0.93 0.7))
       (lin-lin freq 0 13000 (* 2.5 freq) (* 4.5 freq))
       (lin-lin freq 0 10000 0.1 1.0)))))

(stop)
(dean)

(defn phrase [root]
  [root
   (shuffle (into [(+ root 2)
                   (+ root 4)] (map #(+ root %)
                                    (take 2 (shuffle (range 5))))))])

(defn break-dur [d]
  (case (rand-int 6)
    0
    [(/ d 2) (/ d 2)]
    1
    [(/ d 2) (/ d 2)]
    2
    [(* d 2/3) (/ d 3)]
    3
    [(/ d 3) (/ d 3) (/ d 3)]
    4
    [(* d 3/4) (/ d 4)]
    5
    [(* d 7/8) (/ d 8)]))


(frequencies
 (repeatedly 1000
             #))

(defn rhythm-next [durs]
  (let [sdur (sort-by #(- (first %)) (map vector durs (range)))
        idx (Math/round (Math/abs (* 1 (overtone.helpers.rand/rand-gaussian))))]
    (if-let [[_ idx] (try (nth sdur idx) (catch IndexOutOfBoundsException _))]
      (mapcat (fn [d i]
                (if (= i idx)
                  (break-dur d)
                  [d]))
              durs (range))
      (recur durs))))

(-> [8]
    rhythm-next
    rhythm-next
    rhythm-next
    rhythm-next
    )
(nth (iterate #'rhythm-next [4]) 2)
(pplay ::y
       (pbind {:instrument sampled-piano
               :degree (with-meta (for [root [1 1 4 5]]
                                    [(phrase root) :rest])
                         {:dur [1/2 1 2]})
               :dur (pdo (let [d (nth (iterate #'rhythm-next [4]) 2)]
                           [d d d d]))
               }))
(stop)
(do
  (pplay ::x
         (pbind {:instrument dean
                 :type :chord
                 :degree (repeat [1 6 2 3 1 4 5 4])
                 :octave 2
                 :dur 8
                 :chord-size [3 4]
                 :inversion [0 -1]
                 :root :g
                 })
         {:quant 8}
         )

  (pplay ::y
         (pbind {:instrument sampled-piano
                 :degree (for [root [1 6 2 3 1 4 5 4]]
                           [(phrase root) :rest])
                 :dur [1/2 1/2 1/2 1/2 1 1 1/2 1 1/2 2]
                 :swing 1/8
                 })
         {:quant (* 8 8)}))
(stop)
(*clock* :bpm 200)

(for [[l & s] '([clojure pprint]
                [clojure.java io]
                [overtone.libs event counters]
                [overtone.sc server info defaults node dyn-vars]
                [overtone.sc.machinery allocator]
                [overtone.sc.machinery.server connection comms]
                [overtone.sc server info]
                [overtone.helpers audio-file lib file doc]
                [overtone.sc util]
                [overtone.config store])
      s s]
  [(symbol (str l "." s)) :as s])

(take 10
      (pbind {:x [1 2 3]} ##Inf))
;; => ({:x 1} {:x 2} {:x 3})

(pbind {:x "a" :y [1 2 3]})
;; => ({:x "a", :y 1} {:x "a", :y 2} {:x "a", :y 3})

(pbind {:x ["a" "b" "c"] :y [1 2 3]})
;; => ({:x "a", :y 1} {:x "b", :y 2} {:x "c", :y 3})

(pbind {:x ["a" "b"] :y [1 2 3]})
;; => ({:x "a", :y 1} {:x "b", :y 2} {:x "a", :y 3})

(pbind {:x [1 2]} 3)
;; => ({:x 1} {:x 2} {:x 1} {:x 2} {:x 1} {:x 2})
(take 5 (pbind {:x (repeatedly rand)}))
;; => ({:x 0.05675691736257649}
;;     {:x 0.7919472289743806}
;;     {:x 0.4249618005485135}
;;     {:x 0.30638021751335287}
;;     {:x 0.5366715450956588})

(definst gloria [freq 265
                 gate 1.0
                 amp 1]
  (splay
   (* (env-gen (adsr :attack 0.009 :decay 0.2 :sustain 0.5) :gate gate :action FREE)
      amp
      (+ (* 0.5 (sin-osc freq))
         (pluck (brown-noise)
                :maxdelaytime 1
                :delaytime (/ 1 (freq-spread freq 3 0.5)))))
   :level-comp? false))

(def phrase1
  [{:note :c :dur 1}
   {:note :d :dur 1/2}
   {:note :e :dur 1}
   {:note :rest :dur 3/4}])

(def phrase2
  [{:note :g :dur 1}
   {:note :c :dur 1/2}
   {:note :d :dur 1}
   {:note :rest :dur 3/4}])

(pplay ::x
       (repeat 3 [phrase1 phrase2])
       {:proto {:instrument gloria
                :octave 4}})


(pplay ::x
       (pbind {:instrument gloria
               :octave     4
               :note       [:c :d :e :rest]
               :dur        [1 1/2 1 3/4]}))

(pbind {:x [["a" "b"] "c" ["d"]]})
;; => ({:x "a"} {:x "b"} {:x "c"} {:x "d"})

(pplay ::x
       (pbind {:instrument gloria
               :octave     4
               :degree     [1 2 3 :rest]
               :root       [:c :c :c :c :g :g :g :g :d :d :d :d]
               :dur        [1 1/2 1 3/4]}))


(pplay ::x
       (pbind {:instrument gloria
               :octave     4
               :degree     (pchoose [:i :v :iv :iii])
               :root       ^{:dur 4} [:c :g :d]
               :dur        [1 1/2 1 3/2]}))
(stop)

(take 10
      (pchoose [:i :v :iv :rest]))

(pfirst 4)

(pwhite 0 9 4)
;; => (7 6 2 6)
(pwhite 0.0 9.0 4)
;; => (4.4967438662345405 8.873872953797134 4.636984289426836 4.874344111015306)
(take 5 (pwhite 0 99))
;; => (17 76 82 29 12)


(pseries 0 2 5)
;; => (0 2 4 6 8)
(take 7 (pseries 0 2))
;; => (0 2 4 6 8 10 12)

(ppad (pbind {:degree     [:i :v :iv]
              :dur        [1 1/2 1]})
      4)
;; => ({:degree :i, :dur 1}
;;     {:degree :v, :dur 1/2}
;;     {:degree :iv, :dur 1}
;;     {:type :rest, :dur 3/2})
(stop)
