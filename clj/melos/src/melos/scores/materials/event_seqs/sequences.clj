(ns melos.scores.materials.event-seqs.sequences
  (:require [melos.tools.utils :as utils]))

(defn unfold-range
  [[start stop]]
  (let [direction (if (> start stop) -1 1)]
    (range start stop direction)))

(defn unfold-ranges
  [& ranges]
  (mapcat unfold-range ranges))

(defn partition-and-interleave-phrases
  [& colls]
  (let [seqs (map (fn [[partitioning coll]]
                    (utils/cyclic-partition partitioning (cycle coll)))
                  colls)]
    (->> (apply interleave seqs)
         (flatten))))

(defn gradually-expand-chord
  [pitches]
  (rest (reductions conj [] pitches)))

(defn gradually-expand-chords
  [& chords]
  (mapcat gradually-expand-chord chords))

(take 100 (partition-and-interleave-phrases [[2 3] [:a :b :c]] [[1] [:d :e]]))
(gradually-expand-chords [0 2 4 5 7] [0 4 5 6 7 8])
(unfold-ranges [0 12] [2 7] [3 30])

(set! *print-length* 100)

(take 100 (partition-and-interleave-phrases [[5 13 7] [0 4]] [[1] [:d :e]]))

;; TODO: concrete pitch materials
;; TODO: classify materials
;; TODO: link pitches to other parameters?
