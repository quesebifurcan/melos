(ns melos.scores.materials.event-seqs.upper
  (:require [melos.scores.tools :as tools]
            [melos.scores.utils :as utils]))

(def pitches
  (let [ranges [
                (range 0 10)
                (range 3 10)
                (range 0 7)
                ]
        partitions (map count ranges)
    ;; (->> (mapcat (fn [x]
    ;;                (take-while (complement empty?)
    ;;                            (tools/cyclic-partition
    ;;                             x
    ;;                             [2 4 3])))
    ;;              ranges)
        pitches (->> (apply concat ranges)
                     (clojure.walk/postwalk
                     (fn [x] (if (number? x) [x] x))))]
    (->> (take-while (complement empty?)
                     (tools/cyclic-partition
                      pitches
                      partitions))
         (map (fn [x] (take-while (complement empty?)
                                  (tools/cyclic-partition
                                   x
                                   [2 4 3])))))))

(defn combine-partitions
  [& partitions]
  (let [cycle-len (apply * (map (fn [x] (apply + x))
                                partitions))
        seqs (map (fn [part]
                    (take-while (fn [x] (< x cycle-len))
                                (reductions + 0 (cycle part))))
                  partitions)]
    (->> (sort (set (apply concat seqs)))
         (partition 2 1)
         (map (fn [[x y]] (- y x))))))

(require '[schema.core :as s])
(require '[melos.tools.schemata :as ms])

(defn ascending
  []
  {:pitch (map (fn [x] [x])
                (concat
                 (range -3 10)
                 (range 10 -3 -1)))
   :part [:upper]
   :fn tools/make-chord-from-pitch-vector-params
   ;; :partition (fn [x] (take-while (complement empty?)
   ;;                                (tools/cyclic-partition x (cycle [4 4 4]))))
   :partition #(tools/cyclic-partition % [4])
   :duration [1/4 1/4 1/4 1/4]})

(defn upper-soft
  []
  {:pitch (map (fn [x] [0 x])
                (concat
                 (range -3 10)
                 (range 10 -3 -1)))
   :part [:upper]
   :dissonance-contributor? [false]
   :fn tools/make-chord-from-pitch-vector-params
   :partition #(tools/cyclic-partition % [1 1 1 2 2 1 1 2])
   :duration [1/4 1/4 1/4 1/4]})

(defn upper []
  {:upper/b (tools/unfold-events (upper-soft))
   :upper/a (tools/unfold-events (ascending))})

;; (tools/cyclic-partition
;;  (tools/cyclic-repeats
;;   [0 2 3]
;;   [5 2 3 4])
;;  [3 4 2])

;; if direction is down, set "step" to -x.
;; partition subpartition


;; (take 10 (tools/unfold-events (ascending)))
(require '[melos.tools.schemata :as ms])
(require '[schema.core :as s])

(map (fn [x] (s/validate [ms/VerticalMoment] x))
     (take 10 (tools/unfold-events (upper-soft))))


;; (combine-partitions [[2 3 4] [7 8 9]])

