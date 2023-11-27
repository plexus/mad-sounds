;; four_on_the_floar #inst "2021-01-13T06:40:20.107-00:00"
(require 'vibeflow.drumcircle)

(swap!
 vibeflow.drumcircle/state
 merge
 {:pattern
  #{[0 :kick] [3/4 :kick] [7/8 :hi-hat-open] [3/8 :hi-hat-open]
    [9/16 :hi-hat] [3/4 :snare] [5/8 :hi-hat-open] [1/8 :hi-hat-open]
    [1/4 :snare] [1/2 :kick] [1/4 :kick]},
  #_#_:instruments
  {:kick [0 40 127],
   :snare [0 53 127],
   :hi-hat [0 71 127],
   :hi-hat-open [0 69 127]}})
