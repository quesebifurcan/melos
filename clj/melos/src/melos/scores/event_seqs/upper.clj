(ns melos.scores.event-seqs.upper
  (:require [melos.scores.tools :as tools]))


(defn upper-soft
  []
  {:pitch
   (map-indexed (fn [i x]
                  (if (= (rem i 9) 0)
                    [x] [x]))
                (concat
                 (range -3 10)
                 (range 10 -3 -1)))
   :dissonance-contributor? [true]
   :part [:upper]
   :fn tools/make-chord-from-pitch-vector-params
   :partition #(tools/cyclic-partition % [1 1 1 2 2 1 1 2])
   :duration [1/4 1/4 1/4 1/4]})
