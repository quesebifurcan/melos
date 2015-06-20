(ns melos.scores.event-seqs.lower
  (:require [melos.scores.tools :as tools]
            [melos.scores.utils :as utils]))

(defn lower-soft
  []
  {:pitch (map utils/maybe-vec
               [-3 9 4 -3 9 16 9 4])
   :part [:lower]
   :fn tools/make-chord-from-pitch-vector-params
   :partition #(tools/cyclic-partition % [1])
   :duration [1/4 1/4 1/4 1/4]})

(defn lower []
  {:a (tools/unfold-events (lower-soft))})
