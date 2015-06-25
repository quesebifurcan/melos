(ns melos.scores.materials.event-seqs.upper
  (:require [melos.scores.utils :as utils]))

(defn ascending
  []
  (let [pitches (concat (range -3 10)
                        (range 10 -3 -1))]
    {:pitch (->> pitches
                 (map utils/maybe-vec)
                 (utils/cyclic-repeats [4]))
     :part [:upper]
     :fn utils/make-chord-from-pitch-vector-params
     :partition (partial utils/cyclic-partition [1])
     :duration [1/4 1/4 1/4 1/4]}))

(defn upper []
   {:upper/a (utils/unfold-events (ascending))})
