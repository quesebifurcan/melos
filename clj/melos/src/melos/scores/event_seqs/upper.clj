(ns melos.scores.event-seqs.upper
  (:require [melos.scores.tools :as tools]
            [melos.scores.utils :as utils]))

(defn upper-soft
  []
  {:pitch (map (fn [x] [0 x])
                (concat
                 (range -3 10)
                 (range 10 -3 -1)))
   :part [:upper]
   :dissonance-contributor? [true]
   :fn tools/make-chord-from-pitch-vector-params
   :partition #(tools/cyclic-partition % [1 1 1 2 2 1 1 2])
   :duration [1/4 1/4 1/4 1/4]})

(defn upper []
  {:upper/a (tools/unfold-events (upper-soft))})
