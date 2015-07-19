(ns melos.scores.materials.event-seqs
  (:require [melos.tools.utils :as utils]))

;; 1. types of contours
;; 2. ranges?
;; 3. scales?

(defn upper
  [part-name transposition]
  (let [pitches (range 16)]
    {:pitch (->> pitches
                 (utils/transpose transposition)
                 (map utils/maybe-vec))
     :part [part-name]
     :allow-extension? [false false true
                        false false true
                        false true]
     :fn utils/make-chord-from-pitch-vector-params
     :partition (partial utils/cyclic-partition [3 3 2])
     :max-part-count [3]
     :duration [1/4]}))

(defn duration-fn
  [group-count]
  (if (= group-count 1)
    [2/4]
    (repeat group-count 1/4)))

(defn allow-extension-fn
  [cnt]
  (concat (repeat (- cnt 1) false)
          [true]))

(defn upper-2
  [part-name transposition]
  (let [total-range (range 0 40)
        bottom [3 2 1 0]
        top [8 12 15]
        pitches (map (fn [b t]
                       (->> total-range
                            (drop-while #(< % b))
                            (take-while #(< % t))))
                     bottom
                     top)
        basic-partition (map count pitches)
        additional-partition (utils/combine-partitions basic-partition
                                                       [3 3 2])]
    {:pitch (->> (flatten pitches)
                 (utils/transpose transposition)
                 (map utils/maybe-vec))
     :part [part-name]
     :allow-extension? (mapcat allow-extension-fn additional-partition)
     :fn utils/make-chord-from-pitch-vector-params
     :partition (partial utils/cyclic-partition additional-partition)
     :duration (mapcat duration-fn additional-partition)
     }))

(defn lower
  [part-name transposition]
  (let [pitches (range 13)]
    {:pitch (->> pitches
                 (utils/transpose transposition)
                 (map utils/maybe-vec))
     :part [part-name]
     ;; :dissonance-contributor? [false]
     :fn utils/make-chord-from-pitch-vector-params
     :partition (partial utils/cyclic-partition [2 1 1])
     :duration [1/4 1/4 1/4 1/4]
     }))

(defn lower-2
  [part-name transposition]
  (let [pitches
        ;; [[0] [0 2] [0 2 4] [0 2 4 5]]]
        [[0] [0 2] [2] [2 4] [4] [4 5] [5]]]
    {:pitch (->> pitches
                 (map (partial utils/transpose transposition)))
     :part [part-name]
     :dissonance-contributor? [true]
     :fn utils/make-chord-from-pitch-vector-params
     :partition (partial utils/cyclic-partition [2 1 1])
     :duration [1/4 1/4 1/4 1/4]
     }))

(defn ped
  [part-name transposition]
  (let [pitches (range 12 0 -1)]
    {:pitch (->> pitches
                 (utils/transpose transposition)
                 (map utils/maybe-vec)
                 (utils/cyclic-repeats [3 3 4 4 5 5 2 2]))
     :part [part-name]
     ;; :dissonance-contributor? [false]
     ;; :max-count [1]
     :fn utils/make-chord-from-pitch-vector-params
     :partition (partial utils/cyclic-partition [1])
     :duration [1/4]
     }))

(defn organ
  []
  {:upper/a
   (utils/unfold-events (upper :upper -3))
   :lower/a
   (utils/unfold-events (lower :lower -1))
   :ped/a
   (utils/unfold-events (ped :ped -20))})

