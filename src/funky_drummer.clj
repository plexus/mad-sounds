;; funky_drummer #inst "2021-01-13T06:53:04.114-00:00"
(require 'vibeflow.drumcircle)

(swap!
 vibeflow.drumcircle/state
 merge
 {:pattern
  #{[0 :kick] [15/16 :hi-hat] [15/16 :snare] [7/16 :snare]
    [7/16 :hi-hat-open] [1/16 :hi-hat] [3/8 :hi-hat] [9/16 :snare]
    [13/16 :kick] [1/8 :hi-hat] [11/16 :snare] [7/8 :hi-hat]
    [9/16 :hi-hat] [3/4 :snare] [1/8 :kick] [1/2 :hi-hat] [5/8 :kick]
    [5/8 :hi-hat] [5/16 :hi-hat] [11/16 :hi-hat] [1/4 :snare]
    [1/4 :hi-hat] [3/4 :hi-hat] [0 :hi-hat] [13/16 :hi-hat-open]
    [3/16 :hi-hat]},
  #_#_:instruments
  {:kick [0 40 127],
   :snare [0 53 127],
   :hi-hat [0 71 127],
   :hi-hat-open [0 69 127]}})
