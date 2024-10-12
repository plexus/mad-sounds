(ns vibeflow.shuttlexpress
  (:require
   [overtone.libs.event :as event])
  (:import
   (java.io File FileInputStream)))

(defn match? [v1 v2]
  (reduce (fn [acc [a b]]
            (or (= a b)
                (and (ifn? a) (a b))
                (reduced false)))
          true (map list v1 v2)))

(defonce last-wheel-pos (volatile! nil))

(defn handle-event [e]
  (let [e (drop 16 e)]
    (println e)
    ;; very quick and dirty reverse engineered events from the Contour ShuttleXpress
    (cond
      (match? [1 0 #{8 7 6 5 4} 1 #{1 0} 0 0 0] e)
      (let [[_ _ button _ press] e]
        (if (= 1 press)
          (event/event :shuttlexpress/button-press :button (- button 4))
          (event/event :shuttlexpress/button-release :button (- button 4))))
      (match? [2 0 7 0 int? 0 0 0] e)
      (let [[_ _ _ _ pos] e
            last-pos @last-wheel-pos]
        ;; pos=0 is never reported, this is apparently a known issue with the linux driver
        (event/event :shuttlexpress/wheel :pos pos :change (if last-pos (- last-pos pos) 0))
        (vreset! last-wheel-pos pos))
      (match? [2 0 8 0 int?] e)
      (let [[_ _ _ _ pos] e]
        ;; pos=0 is never reported, this is apparently a known issue with the linux driver
        (event/event :shuttlexpress/jog :pos pos)))))

(defn activate-shuttlexpress-events []
  (let [is (FileInputStream. (File. "/dev/input/by-id/usb-Contour_Design_ShuttleXpress-event-mouse"))
        bs (byte-array 24)]
    (future
      (while true
        ;; It appears we're getting 24 byte events, although for each button
        ;; press/wheel change we might get three or four of these. The first 16
        ;; bytes appear to be a timestamp, the remaining 8 we can eyeball to
        ;; find the bits that matter for us.
        (handle-event (take (.read is bs 0 24) bs))))))

(event/event-debug-on)

(activate-shuttlexpress-events)
