(ns melos.scores.materials.melody-sources
  (:require [melos.scores.materials.event-seqs :refer [pendulum-1
                                                       pendulum-2
                                                       lindenmayer-1]]))

(defn organ
  []
  ;; TODO: return lazy seqs.
  {:upper {:a (pendulum-1 :upper 3)}
  ;; {:upper {:a (tes/unfold-events (tes/morph))}
   :lower {:a (pendulum-2 :lower)}
   :ped {:a (lindenmayer-1 :ped 22)
         :b (lindenmayer-1 :ped 7)}})
