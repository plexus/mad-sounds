(ns mad-sounds.sessions.aarschot-2024-04-20-patterns-next
  (:require
   [overtone.studio.pattern :as p]))

(p/pbind {:note [1 2 3 4]
          :dur [1/2 1 1/2]
          :mtranspose ^{:dur [4 2 4]} [0 5 7]
          })

(p/pnext )

(map meta
     (take-while identity
                 (iterate p/pnext ^{:dur [4 2 4]} [0 5 7])))
