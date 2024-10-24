(ns vibeflow.effects
  (:use overtone.live))

(defsynth reverb [in-bus 0
                  out-bus 0
                  level 1
                  roomsize 2
                  revtime 2.0
                  damping 0.75
                  inputbw 0.5
                  spread 15.0
                  drylevel 0
                  earlyreflevel 0.7
                  taillevel 0.5
                  maxroomsize 300.0]
  (out out-bus
       (* level
          (g-verb (in in-bus) roomsize revtime damping inputbw spread drylevel earlyreflevel taillevel maxroomsize))))

(comment
  (def rvb (reverb (aux-bus :reverb)))
  (ctl rvb :roomsize 2)

  (aux-ctl inst-a :reverb 0.2)
  (aux-ctl inst-b :reverb 0.3)

  )
