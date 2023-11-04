(ns mad-sounds.euclid
  "Euclidean Rhythm
  https://en.wikipedia.org/wiki/Euclidean_rhythm

  Implementation mostly nabbed from this, since the algorithm outlined on
  Wikipedia doesn't really make sense.

  https://github.com/dbkaplun/euclidean-rhythm
  ")

(defn euclid-pattern
  "Pattern as a sequence of true/false values"
  [total notes]
  (apply
   concat
   (loop [groups (map #(do [(< % notes)]) (range total))]
     (let [grp-cnt (dec (count groups))
           grp-a   (first groups)
           grp-z   (last groups)
           start   (min grp-cnt (count (take-while #(= % grp-a) groups)))
           end     (dec (count (drop-while #(= % grp-z) (reverse groups))))
           cnt     (min start (- grp-cnt end))]
       (if (or (= start grp-cnt)
               (= 0 grp-cnt)
               (= 0 end))
         groups
         (recur
          (concat (map-indexed (fn [idx group]
                                 (concat group (nth groups (- grp-cnt idx))))
                               (take cnt groups))
                  (take (inc (- grp-cnt cnt cnt)) (drop cnt groups)))))))))

(defn euclid-beats
  "Pattern converted to beat numbers, as can be fed to a metronome

  (euclid-beats 13 5) ;=> (0 3 5 8 10)"
  [total notes]
  (keep-indexed (fn [idx x]
                  (when x idx))
                (euclid-pattern total notes)))

(defn euclid-seq
  "Infinite seq of beat numbers

  (take 20 (euclid-seq 13 5))
  ;;=> (0 3 5 8 10 13 16 18 21 23 26 29 31 34 36 39 42 44 47 49)"
  [total notes]
  (concat (euclid-beats total notes)
          (lazy-seq
           (map (partial + total)
                (euclid-seq total notes)))))
