(ns melos.scores.materials.event-seqs
  (:require [melos.tools.utils :as utils]
            [melos.tools.cycle-params :as cycle-params]))

;; 1. types of contours
;; 2. ranges?
;; 3. scales?

;; scale, ladder, unfolding chords.
;; up, down, pyramid.

(defn ladder
  [pitches]
  (let [stop (last pitches)]
    (->> pitches
         (partition 2 1)
         (mapcat (fn [[x y]] [[x] [x y]]))
         (into [])
         (#(conj % [stop])))))

(defn filter-by-pcs
  [pcs scale]
  (if (empty? pcs)
    scale
    (filter (fn [x] (contains? pcs (rem x 12)))
            scale)))

(defn mirror
  [xs]
  (concat xs
          ((comp butlast rest reverse) xs)))

(defn asdf
  [{:keys [r pcs] :or {:r 10 :pcs #{}}}]
  (->> (range r)
       (filter-by-pcs pcs)
       (mirror)
       (ladder)))

(def upper-pitches
  (mapcat asdf
          (cycle-params/unfold-parameter-cycles
           [{:values [20]
             :path [:r]
             :cycle [1]}
            {:values [#{0 2 7}]
             :path [:pcs]
             :cycle [1]}]
           4)))

(defn duration-fn
  [group-count]
  (if (= group-count 1)
    [2/4]
    (repeat group-count 1/4)))

(defn allow-extension-fn
  [cnt]
  (concat (repeat (- cnt 1) false)
          [true]))

(defn allow-extension-fn-2
  [pitch]
  (contains? #{0 2 4 7 10} pitch))

(defn repeat-pitchclasses
  [pcs cnt pitches]
  (mapcat (fn [x]
            (if (contains? (set pcs) (rem x 12))
              (repeat cnt x)
              [x]))
          pitches))

(defn pcs-partition
  [pitches pcs]
  (map count
       (mapcat (fn [x]
                 (if (and (= (count (set x)) 1)
                          (contains? pcs (first x)))
                   (map (fn [y] [y]) x)
                   [x]))
               pitches)))

(defn asdf 
  [pcs pitches]
  (if (every? #(contains? pcs %) pitches)
    (map (fn [x] [x]) pitches)
    [pitches]))

;; (defn partition-by-start-pcs
;;   [pitches pcs]
;;   (let [result
;;         (concat [[(first pitches)]]
;;                 (map (comp flatten concat)
;;                      (partition 2 2 []
;;                                 (partition-by #(contains? pcs (rem % 12))
;;                                               (rest pitches)))))]
;;     (mapcat (partial asdf pcs) result)))

(defn partition-by-start-pcs
  [pitches pcs coll result]
  (if (empty? pitches)
    result
    (let [head (first pitches)]
      (cond (contains? pcs (rem head 12))
            (recur (rest pitches)
                   pcs
                   []
                   (conj result (conj coll head)))
            :else
            (recur (rest pitches)
                   pcs
                   (conj coll head)
                   result)))))

(partition-by-start-pcs (range 20)
                        #{0 2 4 7 8}
                        []
                        [])

;; (defn partition-by-start-pcs
;;   [pitches pcs]
;;   (let [result (map (comp flatten concat)
;;                     (partition 2 2 []
;;                                (partition-by #(contains? pcs (rem % 12))
;;                                              (rest pitches))))]
;;     (concat [
;;              (concat [(first pitches)]
;;                      (first result))
;;              ]
;;             (rest result))
;;     ))

(defn upper
  [part-name transposition]
  (let [ranges [
                ;; (range 8)
                ;; (reverse (range 14))
                (range 17)
                ;; (reverse (range 16))
                ;; (range 11)
                ;; (reverse (range 11))
                ]
        ;; ranges (map (partial filter-by-pcs #{0 2 5 7}) ranges)
        pitches (apply concat ranges)
        partition-limit (map count ranges)
        ;; partitions (utils/combine-partitions partition-limit
        ;;                                      [3 3 2])]
        partitions [3 3 2 3 2 3]
        ;; pitches (repeat-pitchclasses #{0 2 4 7 10} 2 pitches)
        ;; pitches (partition-by #(contains? #{0 4 7 10} %) pitches)
        pitches (partition-by-start-pcs pitches #{0 2 4 7 8 11} [] [])
        ;; partitions (pcs-partition pitches #{0 2 4 7 10})]
        partitions (map count pitches)]
    {:pitch (->> (flatten pitches)
                 (utils/transpose transposition)
                 (map utils/maybe-vec))
     :part [part-name]
     ;; :allow-extension? (mapcat allow-extension-fn [3 3 2 3])
     ;; :allow-extension? (map allow-extension-fn-2 pitches)
     :fn utils/make-chord-from-pitch-vector-params
     :partition (partial utils/cyclic-partition partitions)
     :max-part-count [1]
     :duration [1/4]}))

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

(defn upper-3
  [part-name transposition]
  (let [pitches [-3 -2 5 2 3 10 3 2 5 -2]]
    {:pitch (->> pitches
                 (utils/transpose transposition)
                 (map utils/maybe-vec))
     :part [part-name]
     ;; :allow-extension? (mapcat allow-extension-fn [3 3 2 3])
     :fn utils/make-chord-from-pitch-vector-params
     :partition (partial utils/cyclic-partition [3])
     :max-part-count [1]
     :duration [1/4]}))

(defn lower
  [part-name transposition]
  (let [pitches (range 13)
        ;; partitions (utils/combine-partitions [(count pitches)]
        ;;                                      [2 1 1 2 1 1 2 1 1 1])]
        partitions [2 1 1 2 1 1 1 2 1]
        pitches (partition-by-start-pcs pitches #{0 2 4 7 10} [] [])
        partitions (map count pitches)]
    {:pitch (->> (flatten pitches)
                 ;; (filter-by-pcs #{0 2 5 7 9})
                 ;; (repeat-pitchclasses #{0 7} 2)
                 (utils/transpose transposition)
                 (map utils/maybe-vec)
                 ;; (utils/cyclic-repeats [1 1 1 1 1 1 1 1 1 1 1 3 1 1 1 1 1 1 8]))
                 )
     :part [part-name]
     ;; :dissonance-contributor? [false]
     :fn utils/make-chord-from-pitch-vector-params
     ;; :allow-extension? (map allow-extension-fn-2 pitches)
     :partition (partial utils/cyclic-partition partitions)
     :duration [1/4]
     }))

(defn lower-2
  [part-name transposition]
  (let [pitches
        [-5 0 2 0 -5 0 2 0 -2 -3 -5 0 2 5 7 9 12]]
    {:pitch (->> pitches
                 (utils/transpose transposition)
                 (map utils/maybe-vec))
     :part [part-name]
     :dissonance-contributor? [true]
     :fn utils/make-chord-from-pitch-vector-params
     :partition (partial utils/cyclic-partition [
                                                 1 1 1 1 1 1 1 1 1 1 1 2 2 1])
     :duration [1/4 1/4 1/4 1/4]
     }))

(defn ped
  [part-name transposition]
  (let [pitches (range 12 0 -1)
        pitches (repeat-pitchclasses #{0 2 4 7} 4 pitches)]

    {:pitch (->> (flatten pitches)
                 ;; (filter-by-pcs #{0 2 4 5 7 9 11})
                 (utils/transpose transposition)
                 (map utils/maybe-vec))
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
   (utils/unfold-events (lower :lower -3))
   :ped/a
   (utils/unfold-events (ped :ped -16))})
