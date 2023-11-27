;; basic_rock #inst "2021-01-13T06:43:19.762-00:00"
(require 'vibeflow.drumcircle)

(swap!
 vibeflow.drumcircle/state
 merge
 {:pattern
  #{[0 :kick] [3/8 :hi-hat] [1/8 :hi-hat] [7/8 :hi-hat] [3/4 :snare]
    [7/8 :kick] [1/8 :kick] [1/2 :hi-hat] [5/8 :kick] [5/8 :hi-hat]
    [1/4 :snare] [1/4 :hi-hat] [1/2 :kick] [3/4 :hi-hat] [0 :hi-hat]
    [3/8 :kick]},
  #_#_:instruments
  {:kick [0 40 127],
   :snare [0 53 127],
   :hi-hat [0 71 127],
   :hi-hat-open [0 69 127]}})
