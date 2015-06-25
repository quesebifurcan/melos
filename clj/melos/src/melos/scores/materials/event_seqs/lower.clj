(ns melos.scores.materials.event-seqs.lower
  (:require [melos.scores.utils :as utils]))

(defn lower-soft
  []
  {:pitch (map utils/maybe-vec
               [-3 9 4 -3 9 16 9 4])
   :part [:lower]
   :fn utils/make-chord-from-pitch-vector-params
   :partition (partial utils/cyclic-partition [1])
   :duration [1/4]})

(defn lower []
  {:lower/a (utils/unfold-events (lower-soft))})
