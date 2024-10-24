(ns mad-sounds.sessions.aarschot-2024-04-20-patterns-next
  (:require
   [overtone.studio.pattern :as p]))

(p/pbind {:note [1 2 3 4]
          :dur [1/2 1 1/2]
          :mtranspose ^{:dur [4 2 4]} [0 5 7]})
;; =>
({:note 1, :dur 1/2, :mtranspose 0}
 {:note 2, :dur 1, :mtranspose 0}
 {:note 3, :dur 1/2, :mtranspose 0}
 {:note 4, :dur 1/2, :mtranspose 0}
 {:note 1, :dur 1, :mtranspose 0}
 {:note 2, :dur 1/2, :mtranspose 0}
 {:note 3, :dur 1/2, :mtranspose 5}
 {:note 4, :dur 1, :mtranspose 5}
 {:note 1, :dur 1/2, :mtranspose 5}
 {:note 2, :dur 1/2, :mtranspose 7})

(p/pnext )

(map meta
     (take-while identity
                 (iterate p/pnext ^{:dur [4 2 4]} [0 5 7])))


(take 4
      (p/pbind
       {:note (p/pwhite 0 12)
        :dur (p/pchoose [1 1/2 1/4])
        :root :c}))

(p/pbind
 {:note (p/pwhite 0 12)
  :dur (p/pchoose [1 1/2 1/4])
  :root :c})
;;=>
({:note 1, :dur 1, :root :c}
 {:note 11, :dur 1/2, :root :c}
 {:note 11, :dur 1/4, :root :c}
 {:note 10, :dur 1, :root :c})
