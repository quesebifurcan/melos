(ns melos.score.combinations
  (:require [clojure.math.combinatorics :as combinatorics]))

(defn unfold-parameters
  [m]
  (map (fn [x] (zipmap (keys m) x))
       (apply combinatorics/cartesian-product (vals m))))

(defn- substitute-tagged-elts
  [tag placeholders form]
  (if (and (vector? form)
           (= (first form) tag))
    (let [placeholder (str (gensym "A__"))
          value (second form)]
      (do (swap! placeholders assoc placeholder value)
          placeholder))
    form))

(defn- replace-if-placeholder
  [replacement-map form]
  (if (and (string? form)
           (contains? replacement-map form))
    (get replacement-map form)
    form))

(defn nested-map-product
  [form]
  (let [placeholders (atom {})
        mod-form (clojure.walk/postwalk
                  (partial substitute-tagged-elts 'unfold placeholders)
                  form)]
    (swap! placeholders unfold-parameters)
    (map (fn [replacement-map]
           (clojure.walk/postwalk
            (partial replace-if-placeholder replacement-map)
            mod-form))
         @placeholders)))

