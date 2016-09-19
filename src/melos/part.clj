(ns melos.part)

(defn filter-events
  [part-name events]
  (filter #(= part-name (:part %)) events))

(defn filter-chord
  [part-name chord]
  (update chord :events (partial filter-events part-name)))

(defn filter-chords
  [part-name chords]
  (map (partial filter-chord part-name) chords))
