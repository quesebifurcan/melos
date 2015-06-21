(ns melos.scores.materials.melody-sources
  (:require [melos.scores.materials.event-seqs :refer [pendulum-1
                                                       pendulum-2
                                                       lindenmayer-1]]
            [melos.scores.tools :as tools]
            [melos.scores.event-seqs.lower :refer [lower]]
            [melos.scores.event-seqs.ped :refer [ped]]
            [melos.scores.event-seqs.upper :refer [upper]]))

(defn organ
  []
  ;; {:upper {:a (:upper/a (upper))}
  ;; :lower {:a (:lower/a (lower))}
  ;; :ped {:a (:ped/a (ped))}
  ;;  })
  (merge (upper)
         (lower)
         (ped)))

;; TODO: merge maps here
;; TODO: repeated intervals work as well as octaves?
