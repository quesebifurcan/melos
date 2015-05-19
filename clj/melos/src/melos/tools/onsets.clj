(ns melos.tools.onsets)

(defn get-melodic-event
  "Retrieve the most recently added event in *vertical-moment*."
  [vertical-moment]
  (first (filter #(= (:count %) 0) vertical-moment)))

(defn get-onsets
  "Given a sequence of durated *events*, calculate the onsets."
  [events]
  (let [melodic-onsets (map get-melodic-event events)]
    (reductions + 0 (map :delta-dur melodic-onsets))))
