(ns melos.scores.materials.event-seqs.upper
  (:require [melos.scores.tools :as tools]
            [melos.scores.utils :as utils]))

(defn ascending
  []
  (let [pitches (concat (range -3 10)
                        (range 10 -3 -1))]
    {:pitch (->> pitches
                 (map utils/maybe-vec)
                 (tools/cyclic-repeats [4]))
     :part [:upper]
     :fn tools/make-chord-from-pitch-vector-params
     :partition #(tools/cyclic-partition [1] %)
     :duration [1/4 1/4 1/4 1/4]}))

(defn upper []
   {:upper/a (tools/unfold-events (ascending))})
