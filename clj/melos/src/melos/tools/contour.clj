(ns melos.tools.contour)

(require '[clojure.math.numeric-tower :as math])
(require '[clojure.math.combinatorics :as combinatorics])

(defn round-to-nearest
  [n base]
  (int (* base (math/round (/ n base)))))
  
(defn get-options
  [center-pitch pc]
  (let [pivot (round-to-nearest center-pitch 12)]
    (cond (= (rem center-pitch 12) pc)
          [(- pivot (- 12 pc)) center-pitch (+ center-pitch 12)]
          :else
          [(- pivot (- 12 pc)) (+ pivot pc)])))

(get-options 62 3)

(defn score-distance
  [pitches]
  (let [distance (reduce + 0
                             (map (fn [x] (Math/abs (- 60 x)))
                      pitches))]
    distance))

(defn remove-unisons
  [step]
  (if (= step 0) 100 step))

(defn score-interval-sizes
  [pitches]
  (let [groups (partition 2 1 pitches)]
    (->> (map (fn [[x y]] (Math/abs (- x y))) groups)
         (map remove-unisons)
         (reduce + 0))))

(defn normalize
  [x min- max-]
  (map (fn [y] (/ (- y min-)
                  (- max- min-)))
       x))

(defn score-segment
  [group]
  (let [distances (map score-distance group)
        distance-min (apply min distances)
        distance-max (apply max distances)
        distances (normalize distances
                             distance-min distance-max)
        intervals (map score-interval-sizes group)
        intervals-min (apply min intervals)
        intervals-max (apply max intervals)
        intervals (normalize intervals
                             intervals-min intervals-max)]
        
    (map (fn [pitches distance interval]
           {:pitches pitches
            :distance distance
            :interval interval})
         group
         distances
         intervals)))

(defn sort-scores
  [scores]
  (sort-by (fn [{:keys [distance interval]}]
                (+ distance interval))
           scores))

(let [mel [0 10 8 7 2 2 2 2 4 3]]
  (->> (map (partial get-options 60) mel)
       (apply combinatorics/cartesian-product)
       ;; (map (fn [x]
       ;;    (do (println x "\n")
       ;;        x)))
       (score-segment) 
       (sort-scores)
       ))

;; pass in earlier stages to improve result
;; (let [a [[0] [987] [1 2] [3 4] [5 6]]]
;;   (apply combinatorics/cartesian-product a))
 
