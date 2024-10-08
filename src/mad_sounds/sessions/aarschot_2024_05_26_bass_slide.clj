(ns mad-sounds.sessions.aarschot-2024-05-26-bass-slide
  (:use overtone.live))

(connect-jack-ports)

(definst bass [freq 70
               brightness 1.1
               attack 0.01
               decay 0.1
               sustain 0.7
               release 0.1
               amp 0.5
               gate 1
               lag 0.1]
  (let [freq freq #_(lag:kr freq lag)
        env  (env-gen (adsr attack decay sustain release)
                      :gate gate
                      :action FREE)]
    (-> (var-saw freq :width 0)
        (rlpf (* brightness freq env))
        (* amp env))))
(kill bass)
(stop)
(pplay
 ::bass
 (pbind {:type   (cons :note (repeat :ctl))
         :degree [:i :i :iii :ii :i :iv :v]
         :dur    (map #(/ % 4) [3 3 4 2 4])
         :root   ^{:dur 8} [:c3 :d3 :d3 :b2 :c3]
         }
        )
 {:proto {:instrument #'bass
          :attack     0.01
          :lag        0.1
          :octave     3
          }})

(ppause ::bass)

(bass :lag 1 :amp 0.5)
(ctl bass :freq 100)
(stop)
(lag-ud)

(event-debug-off)

(params-vec {:instrument #'bass
             :freq 440
             :attack 0.02
             :foo 1})

(clojure.repl/apropos "node")

(node-tree)