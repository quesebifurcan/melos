(ns melos.scores.materials.event-seqs
  (:require [melos.tools.utils :as utils]))

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

(defn lower-soft
  []
  {:pitch (map utils/maybe-vec
               [-3 9 4 -3 9 16 9 4])
   :part [:lower]
   :fn utils/make-chord-from-pitch-vector-params
   :partition (partial utils/cyclic-partition [1])
   :duration [1/4]})

(defn ped-soft
  []
  (let [pitches (range -17 -21 -1)]
    {:pitch (->> pitches
                 (map utils/maybe-vec)
                 (utils/cyclic-repeats [2 3 4]))
     :part [:ped]
     :fn utils/make-chord-from-pitch-vector-params
     :partition (partial utils/cyclic-partition [1])
     :duration [1/4]}))

(defn organ
  []
  {:upper/a (utils/unfold-events (ascending))
   :lower/a (utils/unfold-events (lower-soft))
   :ped/a (utils/unfold-events (ped-soft))})
