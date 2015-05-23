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
  [center-pitches pitches]
  (let [distance (reduce + 0 (map (fn [x y]
                                    (Math/abs (- x y)))
                                  center-pitches
                                  pitches
                                  ))]
    distance))

(defn remove-unisons
  [step]
  (cond (= step 0)
        1000
        (> step 12)
        100
        :else
        step))

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
  [old-center-pitches old-group center-pitches group]
  (let [
        ;; center-pitches (concat old-center-pitches
        ;;                        center-pitches)
        ;; group (concat old-group group)
        distances (map (fn [x]
                         (score-distance
                          (concat old-center-pitches
                                  center-pitches)
                          (concat old-group
                                  x)))
                       group)
        distance-min (apply min distances)
        distance-max (apply max distances)
        distances (normalize distances
                             distance-min distance-max)
        intervals (map (fn [x]
                         (score-interval-sizes
                          (concat old-group x)))
                       group)
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

(defn find-contour
  [melody contour old-contour old-melody]
  (->> (map get-options contour melody)
       (apply combinatorics/cartesian-product)
       (score-segment old-contour old-melody contour)
       (sort-scores)
       (first)
       (:pitches)))

(defn find-contours
  [melody contour melody-coll contour-coll]
  (if (empty? melody)
    melody-coll
    (recur (rest melody)
           (rest contour)
           (concat melody-coll
                   (find-contour (first melody)
                                 (first contour)
                                 melody-coll
                                 contour-coll))
           (concat contour-coll (first contour)))))

(let [mel (partition 4 4 [] [0 2 4 7 0 2 4 7 0 2 4 7 0 2 4 7])
      ;; contour (partition 4 4 [] (cycle (range 60 80 3)))]
      contour (partition 4 4 [] (cycle [65 65 65 65 70 70 70]))]
  (find-contours mel contour [] []))


;; pass in earlier stages to improve result
;; (let [a [[0] [987] [1 2] [3 4] [5 6]]]
;;   (apply combinatorics/cartesian-product a))
 
