(ns melos.scores.materials.event-seqs.instruments
  (:require [melos.scores.materials.event-seqs.lower :refer [lower]]
            [melos.scores.materials.event-seqs.ped :refer [ped]]
            [melos.scores.materials.event-seqs.upper :refer [upper]]))

(defn organ
  []
  (merge (upper)
         (lower)
         (ped)))

