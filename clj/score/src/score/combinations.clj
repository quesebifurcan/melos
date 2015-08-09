(ns score.combinations
  (:require [clojure.math.combinatorics :as combinatorics]))

(defn unfold-parameters
  [m]
  (map (fn [x] (zipmap (keys m) x))
       (apply combinatorics/cartesian-product (vals m))))
