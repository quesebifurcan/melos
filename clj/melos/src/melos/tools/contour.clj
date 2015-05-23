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

;; pass in earlier stages to improve result
;; (let [a [[0] [987] [1 2] [3 4] [5 6]]]
;;   (apply combinatorics/cartesian-product a))
 
