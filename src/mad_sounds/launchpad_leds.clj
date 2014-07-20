(ns mad-sounds.launchpad-leds
  (:require [mad-sounds.launchpad-mini :refer :all])
  (:require [overtone.studio.midi :refer :all]))

(def led-colors [:red :green :yellow :orange :amber])
(def flags {:ignore 0
            :clear 8
            :copy 12})

(defn velocity [color intensity mode]
  (if (some #{color} led-colors)
    (let [intensity (if (> intensity 3) 3 intensity)
          green (case color
                  :green intensity
                  :yellow intensity
                  :orange 2
                  :amber intensity
                  0)
          red (case color
                :red intensity
                :yellow 2
                :orange intensity
                :amber intensity
                0)
          mode (or mode :copy)]
      (+ (* 16 green)
         red
         (mode flags)))
    0))

(defn led-on [launchpad x y & {:keys [color intensity mode] :or {color :red intensity 3 mode :copy}}]
  (midi-note-on (:rcv launchpad) (coordinate->note x y) (velocity color intensity mode)))

(defn led-off [launchpad x y]
  (midi-note-on (:rcv launchpad) (coordinate->note x y) 0))
