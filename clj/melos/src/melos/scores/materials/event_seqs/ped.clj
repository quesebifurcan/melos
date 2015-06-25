(ns melos.scores.materials.event-seqs.ped
  (:require [melos.scores.tools :as tools]
            [melos.scores.utils :as utils]))

(defn ped-soft
  []
  {:pitch (map utils/maybe-vec
               [
                -17
                -17
                -17
                -17
                -17
                -18
                -18
                -18
                -18
                -19
                -19
                -19
                -19
                -20
                -20
                -20
                -20
                -20
                ])
   :part [:ped]
   :fn utils/make-chord-from-pitch-vector-params
   :partition #(utils/cyclic-partition [1 1 1 2 2 1 1 2] %)
   :duration [1/4 1/4 1/4 1/4]})

(defn ped []
  {:ped/a (tools/unfold-events (ped-soft))})

