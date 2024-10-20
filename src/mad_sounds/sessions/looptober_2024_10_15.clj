(ns sessions.looptober-2024-10-15
  (:use overtone.live))

(def snare (freesound 26903))
(def kick (freesound 2086))
(def close-hihat (freesound 802))
(def open-hihat (freesound 26657))
(def stick (freesound 628568))

(ploop :hat
       {:instrument close-hihat
        :dur [1/2]
        :amp (interleave (pwhite 0.4 0.7)
                         (pwhite 0.6 0.9))})

(ploop :snare
       {:instrument snare
        :note (pchoose [[0 :- :- :-
                         :- :- 0  :-
                         0 :- 0 :-
                         0 :- :- :-]
                        [0 :- :- :-
                         :- :- :-  :-
                         :- 0 :- :-
                         0 :- :- :-]])
        :dur 1/4
        :amp (pwhite 0.7 0.9)})

(ploop :kick
       {:instrument kick
        :note [0 :- :- :-
               0 :-  :- :-
               0 :-  :- 0
               0 :-  :- :-
               ]
        :dur 1/4
        :amp (pwhite 0.4 0.6)
        })

(ploop :stick
       {:instrument stick
        :note [0 :- :- :-
               :- 0 :- :-
               0 :- :- :-
               :- :- 0 :-]
        :dur [1/4]
        :amp 0.2})

(definst pad [freq 300 amp 1 gate 1]
  (let [freq (for [i (range 4)]
               (* freq (rand-in-range 1 1.03)))
        mix (lin-lin (sin-osc 0.5) -1 1 0 1)]
    (splay (* amp
              (env-gen (adsr 0.2 0.1 1 0.3) :gate gate :action FREE)
              (+ (* (+ 0.1 mix) (hpf (var-saw (lin-lin (sin-osc 4)
                                                       -1 1
                                                       (/ freq 1.02)
                                                       (* freq 1.02)))
                                     (* 3 freq)))
                 (* (- 1.1 mix) (lpf (square freq) (* 2 freq))))))))

(pad)

(inst-fx! pad fx-reverb)
(inst-fx! pad fx-distortion-tubescreamer)
(clear-fx pad)

(ploop :pad {:note [0 0 3 -1]
             :dur 4
             :instrument pad})

(ploop :tinkle {:note [[0 3 5 7 0]
                       [0 3 5 3 0]
                       [3 5 3 0 0]
                       [0 3 7 5 0]]
                :octave 6
                :dur [[1/2 1/2 1/4 1/2 1/4]
                      [1/2 1/2 1/2 1/4 1/4]]
                :instrument pad})

(run! ppause [:kick :snare :stick])
(run! presume [:kick :snare :stick :pad])
(run! ppause [:pad :tinkle :hat])
