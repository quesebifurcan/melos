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

;; (get-options 62 3)

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
                          (concat (take-last 4
                                             old-center-pitches)
                                  center-pitches)
                          (concat (take-last 4 old-group)
                                  x)))
                       group)
        distance-min (apply min distances)
        distance-max (apply max distances)
        distances (normalize distances
                             distance-min distance-max)
        intervals (map (fn [x]
                         (score-interval-sizes
                          (concat (take-last 4 old-group) x)))
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

(def find-contour-memoized
  (memoize find-contour))

;; (defn find-contours
;;   [melody contour melody-coll contour-coll]
;;   (if (empty? melody)
;;     melody-coll
;;     (recur (rest melody)
;;            (rest contour)
;;            (concat melody-coll
;;                    (find-contour (first melody)
;;                                  (first contour)
;;                                  ;; (take 10 melody-coll)
;;                                  melody-coll
;;                                  contour-coll))
;;            (concat contour-coll (first contour)))))

(defn find-contours
  [melody contour melody-coll contour-coll]
    (let [result
          (find-contour-memoized (first melody)
                                 (first contour)
                                 ;; (take 10 melody-coll)
                                 melody-coll
                                 contour-coll)]
      (lazy-seq (cons result
                      (find-contours
                       (rest melody)
                       (rest contour)
                       (concat (take-last 3 melody-coll) result)
                       (concat (take-last 3 contour-coll) (first contour)))))))

(defn apply-contour-to-melody
  [melody contour]
  (let [mel (partition 4 4 [] melody)
        contour (partition 4 4 [] contour)]
    (find-contours mel contour [] [])))

(time
(doall (take 1000 (apply-contour-to-melody
 (cycle [0 2 4 7])
 (cycle [0]))
 )))

