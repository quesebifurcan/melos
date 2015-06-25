(ns melos.scores.materials.event-seqs.ped
  (:require [melos.tools.utils :as utils]))

(defn ped-soft
  []
  (let [pitches (range -17 -21 -1)]
    {:pitch (->> pitches
                 (map utils/maybe-vec)
                 (utils/cyclic-repeats [2 3 4]))
     :part [:ped]
     :fn utils/make-chord-from-pitch-vector-params
     :partition (partial utils/cyclic-partition [1])
     :duration [1/4]}))

(defn ped []
  {:ped/a (utils/unfold-events (ped-soft))})

