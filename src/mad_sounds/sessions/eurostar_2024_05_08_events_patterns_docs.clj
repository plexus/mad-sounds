(ns mad-sounds.sessions.eurostar-2024-05-08-events-patterns-docs
  (:use overtone.live))

(now)

(event :my-event :foo "asdf" :bar 200.0)
add-watch

(on-event :my-event println ::print)
(do
  (on-event :special-moment
            (fn [_]
              (println "special moment")
              :overtone/remove-handler)
            ::k)

  ;; Fire the :special-moment event in 3 and 4 seconds
  (after-delay 3000 #(event :special-moment))
  (after-delay 4000 #(event :special-moment))

  )

(event-debug-on)

(defsynth foo []
  (sin-osc))

(midi-connected-devices)

(definst example [freq 265 gate 1 amp 1]
  (* (env-gen (adsr) :gate gate :action FREE)
     amp
     your-sound-source))

(gloria)
(eget {:instrument gloria
       :degree :iii
       :mode :minor
       :octave 5
       :root 0}
      :midinote)
(stop)
(gloria (midi->hz 64))

(def m (metronome 90))

(do
  (event :note {:instrument gloria
                :degree 5
                :bpm 10
                })
  )
(stop)

(let [clock (metronome 120)]
  (dotimes [i 3]
    (event :note {:instrument gloria
                  :clock clock
                  :beat (+ i (clock))
                  :dur 1/2})))

(pplay ::x
       [{:instrument gloria
         :note       :c4
         :dur        1}
        {:instrument gloria
         :note       :d4
         :dur        1/2}
        {:instrument gloria
         :note       :e4
         :dur        1}])

(gloria)
(stop)
(note-info :c4)
{:match "c",  :pitch-class :C, :interval 0}
{:match "c4", :pitch-class :C, :interval 0, :octave 4, :midi-note 60}

(eget {:root :c5} :midinote)
(eget {:degree 1} :scale-notes)

(map #(eget % :freq)
     [
      ])

(eget {:midinote 60} :freq)
;; => 261.6255653005986
