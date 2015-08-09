(ns score.combinations
  (:require [clojure.math.combinatorics :as combinatorics]))

(defn unfold-parameters
  [m]
  (map (fn [x] (zipmap (keys m) x))
       (apply combinatorics/cartesian-product (vals m))))

(defn nested-map-product
  [x]
  (let [coll (atom {})
        mod-form (clojure.walk/prewalk
                  (fn [form]
                    (if (and (vector? form)
                             (= (first form) 'unfold))
                      (let [placeholder (str (gensym "A__"))
                            value (rest form)]
                        (do (swap! coll assoc placeholder value)
                            placeholder))
                      form))
                  x)]
    (let [placeholders @coll]
      (swap! coll unfold-parameters)
      (map (fn [z]
             (clojure.walk/prewalk
              (fn [form]
                (if (and (string? form)
                         (contains? placeholders form))
                  (get z form)
                  form))
              mod-form))
           @coll))))

(nested-map-product {:a '[unfold 1 2 3]
                     :oij {:ui [98] :ahfd '[unfold 7 6 8 8 7 6 5 4 3 33 3 3]}
                     :b {:testing {:a '[unfold 98 76 23]}}})
