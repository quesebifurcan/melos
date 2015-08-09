(ns score.materials.event-seqs
  (:require [melos.utils
             [cycle-params :as cycle-params]
             [utils :as utils]]))

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
  [pcs pitch]
  (contains? pcs (rem pitch 12)))

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

(defn partition-by-interval
  [pitches]
  (map (fn [x]
         (map first x))
       (partition-by #(< (first %) 3)
                     (map (fn [a b]
                            [a b])
                          pitches
                          (map (fn [x]
                                 (Math/abs (apply - x)))
                               (partition 2 1 [] pitches))))))

(defn upper
  [part-name transposition]
  (let [ranges [
                ;; (range 8)
                ;; (reverse (range 14))
                (range 13)
                (range 10)
                (range 8)
                (range 10)
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
        pcs #{0 2 3 5 7 9 10}
        pitches (partition-by-start-pcs pitches pcs [] [])
        ;; partitions (pcs-partition pitches #{0 2 4 7 10})]
        partitions (map count pitches)]
    {:pitch (->> (flatten pitches)
                 (utils/transpose transposition)
                 (map utils/maybe-vec))
     :part [part-name]
     :allow-extension? (map (fn [x]
                              (contains? pcs (rem (+ 60 x) 12)))
                            (flatten pitches))
     ;; :allow-extension? (map (partial allow-extension-fn-2 pcs) (flatten pitches))
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
  (let [ranges [
                 (range 0 11)
                 ]
        ;; partitions (utils/combine-partitions [(count pitches)]
        ;;                                      [2 1 1 2 1 1 2 1 1 1])]
        partition-limit (map count ranges)
        pitches (apply concat ranges)
        partitions [2 1 1 2 1 1 1 2 1]
        pcs #{0 2 5 7}
        pitches (partition-by-start-pcs pitches pcs [] [])
        pitches (mapcat partition-by-interval pitches)
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

(defn filter-curr
  [count-limit curr]
  (if (<= (count curr) count-limit)
    curr
    (recur count-limit
           (butlast curr))))

(defn sustain-pitchclasses
  [pcs curr coll pitches]
  (if (empty? pitches)
    coll
    (let [head (first pitches)
          curr (->> curr
                    (filter #(contains? pcs (rem (+ 60 %) 12)))
                    (filter-curr 1))]
      (recur pcs
             (conj curr head)
             (conj coll (conj curr head))
             (rest pitches)))))

(defn ped
  [part-name transposition]
  (let [pitches (range 12 0 -1)
        pitches (repeat-pitchclasses #{0 2 4 7} 4 pitches)]

    {:pitch (->> (flatten pitches)
                 ;; (filter-by-pcs #{0 2 4 5 7 9 11})
                 (utils/transpose transposition)
                 (sustain-pitchclasses #{9 4} [] []))
                 ;; (map utils/maybe-vec))
     :part [part-name]
     :dissonance-contributor? [false]
     ;; :max-count [5]
     :max-part-count [2]
     ;; :allow-extension? (map (fn [x]
     ;;                          (contains? #{0 2 4 7} (rem (+ 60 x) 12)))
     ;;                        (flatten pitches))
     :fn utils/make-chord-from-pitch-vector-params
     :partition (partial utils/cyclic-partition [1])
     :duration [1/4]
     }))

(defn ped-2
  [part-name transposition]
  (let [ranges [
                ;; (range 8)
                ;; (reverse (range 14))
                (range 0 15)
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
        pcs #{0 2 5 7 9}
        pitches (partition-by-start-pcs pitches pcs [] [])
        ;; partitions (pcs-partition pitches #{0 2 4 7 10})]
        partitions (map count pitches)]
    {:pitch (->> (flatten pitches)
                 (utils/transpose transposition)
                 (map utils/maybe-vec))
     :part [part-name]
     ;; :allow-extension? (mapcat allow-extension-fn [3 3 2 3])
     ;; :allow-extension? (map (partial allow-extension-fn-2 pcs) (flatten pitches))
     :fn utils/make-chord-from-pitch-vector-params
     :partition (partial utils/cyclic-partition partitions)
     :max-part-count [1]
     :duration [1/4]}))

(defn chromatic-cycle
  []
  [
   0 12
   0 7 12 7
   0 5 7 12 7 5
   0 2 5 7 12 7 5 2
   0 2 5 7 10 12 10 7 5 2
   0 2 5 7 9 10 12 10 9 7 5 2
   0 2 4 5 7 9 10 12 10 9 7 5 4 2
   0 2 3 4 5 7 9 10 12 10 9 7 5 4 3 2
   0 2 3 4 5 6 7 9 10 12 10 9 7 6 5 4 3 2
   0 2 3 4 5 6 7 9 10 11 12 11 10 9 7 6 5 4 3 2
   ])

(defn chromatic-cycle-seed
  []
  [
   0 12
   0 12
   0 12
   0 12
   0 12
   0 12
   0 12
   0 12
   0 12
   0 12
   0 12
   0 12
   0 12
   0 12
   0 12
   0 7 12 7
   0 7 12 7
   0 7 12 7
   0 7 12 7
   0 7 12 7
   0 7 12 7
   0 7 12 7
   0 7 12 7
   0 7 12 7
   0 7 12 7
   0 7 12 7
   0 7 12 7
   0 5 7 12 7 5
   0 5 7 12 7 5
   0 5 7 12 7 5
   0 5 7 12 7 5
   0 5 7 12 7 5
   0 5 7 12 7 5
   0 5 7 12 7 5
   0 5 7 12 7 5
   0 5 7 12 7 5
   0 2 5 7 12 7 5 2
   0 2 5 7 12 7 5 2
   0 2 5 7 12 7 5 2
   0 2 5 7 12 7 5 2
   0 2 5 7 12 7 5 2
   0 2 5 7 12 7 5 2
   0 2 5 7 12 7 5 2
   0 2 5 7 10 12 10 7 5 2
   0 2 5 7 10 12 10 7 5 2
   0 2 5 7 10 12 10 7 5 2
   0 2 5 7 10 12 10 7 5 2
   0 2 5 7 10 12 10 7 5 2
   0 2 5 7 10 12 10 7 5 2
   0 2 5 7 9 10 12 10 9 7 5 2
   0 2 5 7 9 10 12 10 9 7 5 2
   0 2 5 7 9 10 12 10 9 7 5 2
   0 2 5 7 9 10 12 10 9 7 5 2
   0 2 5 7 9 10 12 10 9 7 5 2
   0 2 4 5 7 9 10 12 10 9 7 5 4 2
   0 2 4 5 7 9 10 12 10 9 7 5 4 2
   0 2 4 5 7 9 10 12 10 9 7 5 4 2
   0 2 4 5 7 9 10 12 10 9 7 5 4 2
   0 2 3 4 5 7 9 10 12 10 9 7 5 4 3 2
   0 2 3 4 5 7 9 10 12 10 9 7 5 4 3 2
   0 2 3 4 5 7 9 10 12 10 9 7 5 4 3 2
   0 2 3 4 5 6 7 9 10 12 10 9 7 6 5 4 3 2
   0 2 3 4 5 6 7 9 10 12 10 9 7 6 5 4 3 2
   0 2 3 4 5 6 7 9 10 11 12 11 10 9 7 6 5 4 3 2
   ])

(defn chromatic-cycle
  []
  (concat (chromatic-cycle-seed)
          (reverse (chromatic-cycle-seed))))

(defn diatonic-cycle-2
  []
  (let [model [

               ;; 0 12
               ;; 0 7 12 7
               ;; 0 5 7 12 7 5
               ;; 0 2 5 7 12 7 5 2
               ;; 0 2 5 7 10 12 10 7 5 2
               ;; 0 2 4 5 7 10 12 10 7 5 4 2
               ;; 0 2 3 4 5 7 10 12 10 7 5 4 3 2
               ;; 0 2 3 4 5 7 10 11 12 11 10 7 5 4 3 2
               ;; 0 2 3 4 5 6 7 10 11 12 11 10 7 6 5 4 3 2
               ;; 0 2 3 4 5 6 7 9 10 11 12 11 10 9 7 6 5 4 3 2
               ;; 0 2 3 4 5 6 7 8 9 10 11 12 11 10 9 8 7 6 5 4 3 2

               ]]
    model
  ;;           (map (partial + 5) model))))
  ))



(defn diatonic-cycle
  []
  [

   0 7
   0 7 12 7
   0 2 7 12 7 2
   0 2 5 7 12 7 5
   0 2 5 7 10 12 10 7 5 2
   0 2 3 5 7 10 12 10 7 5 3 2

   0 7
   0 7 12
   0 2 7 12
   0 2 5 7 12
   0 2 5 7 10 12
   0 2 3 5 7 10 12

   7
   12 7
   12 7 2
   12 7 5
   12 10 7 5 2
   12 10 7 5 3 2
   

   ;; 0 7
   ;; 0 3 7 3
   ;; 0 2 3 7 3 2

   ;; 0 5
   ;; 0 5 7 5
   ;; 0 5 7 8 7 5

   ;; 0 3
   ;; 0 2 3 2
   ;; 0 2 3 7 8 7 3 2

   ;; add notes in different order.

   ;; 0 2
   ;; 0 2 3 2
   ;; 0 2 3 5 3 2
   ;; 0 2 3 5 7 5 3 2
   ;; 0 2 3 5 7 10 7 5 3 2
   ;; 0 2 3 5 7 10 12 10 7 5 3 2
   ;; 0 2 3 5 7 10 12 14 12 10 7 5 3 2
   ;; 0 2 3 5 7 10 12 14 15 14 12 10 7 5 3 2

   ;; 3
   ;; 3 10
   ;; 3 5 10 5
   ;; 2 3 5 10 5 3
   ;; 2 3 5 10 12 10 5 3
   ;; 2 3 5 7 10 12 10 7 5 3 2
   ;; 0 2 3 5 7 10 12 10 7 5 3 2

   ])

;; (def diatonic-rhythm
;;   [pitch-group]
;;   (println (count pitch-group))
;;   1/4)

(defn diatonic-upper
  [part-name transposition]
  (let [pitches (diatonic-cycle)
        partitions [2]]
    {:pitch (->> pitches
                 (utils/transpose transposition)
                 (map utils/maybe-vec))
     :part [part-name]
     :fn utils/make-chord-from-pitch-vector-params
     :partition (partial utils/cyclic-partition partitions)
     :max-part-count [1]
     :duration [1/4]}))

(defn diatonic-lower
  [part-name transposition]
  (let [pitches (diatonic-cycle)
        partitions [3]]
    {:pitch (->> pitches
                 (utils/transpose transposition)
                 (map utils/maybe-vec))
     :part [part-name]
     :allow-extension? [false false true]
     :fn utils/make-chord-from-pitch-vector-params
     :partition (partial utils/cyclic-partition partitions)
     :max-part-count [1]
     :duration [1/4]}))

(defn diatonic-ped
  [part-name transposition]
  (let [pitches (diatonic-cycle)
        partitions [1]]
    {:pitch (->> pitches
                 (utils/transpose transposition)
                 (map utils/maybe-vec))
     :part [part-name]
     :fn utils/make-chord-from-pitch-vector-params
     :partition (partial utils/cyclic-partition partitions)
     :max-part-count [1]
     :duration [1/4]}))

(defn organ
  []
  ;; {:upper/a
  ;;  (utils/unfold-events (diatonic-upper :upper -3))
  ;;  :lower/a
  ;;  (utils/unfold-events (diatonic-lower :lower -3))
  ;;  :ped/a
  ;;  (utils/unfold-events (diatonic-ped :ped -15))})
  {
   :upper/a
   (utils/unfold-events (upper :upper -3))
   :lower/a
   (utils/unfold-events (lower :lower -3))
   :ped/a
   (utils/unfold-events (ped-2 :ped -15))})

;; TODO: After unfolding all (approximated) partials, gradually "shift weight" from lower to higher partials.
;; TODO: Chromatic canon? Which extensions/modifications?
;; TODO: Duration? Strict process?
