(ns repl-session.poke-hid
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

(defn handle-event [e]
  (let [e (drop 16 e)]
    ;; very quick and dirty reverse engineered events from the Contour ShuttleXpress
    (cond
      (match? [1 0 #{8 7 6 5 4} 1 #{1 0} 0 0 0] e)
      (let [[_ _ button _ press] e]
        (if (= 1 press)
          (event/event :shuttlexpress/button-press {:button (- button 4)})
          (event/event :shuttlexpress/button-release {:button (- button 4)})))
      (match? [2 0 7 0 int? 0 0 0] e)
      (let [[_ _ _ _ pos] e]
        (event/event :shuttlexpress/wheel {:pos pos}))
      (match? [2 0 8 0 int?] e)
      (let [[_ _ _ _ pos] e]
        (event/event :shuttlexpress/jog {:pos pos})))))

(defn activate-shuttlexpress-events []
  (let [is (FileInputStream. (File. "/dev/input/by-id/usb-Contour_Design_ShuttleXpress-event-mouse"))
        bs (byte-array 24)]
    (future
      (while true
        (handle-event (take (.read is bs 0 24) bs))))))
