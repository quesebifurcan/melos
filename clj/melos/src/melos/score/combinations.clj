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

;; weave-seqs helper functions

(defn- get-highest-keys
  [m]
  (let [max-val (apply max (vals m))]
    (->> m
         (filter (fn [[k v]] (= v max-val)))
         (keys))))

(defn- get-next-key
  [m exclude]
  (let [candidates (set (get-highest-keys m))]
    (if (> (count candidates) 1)
      (first (clojure.set/difference candidates exclude))
      (first candidates))))

(defn- count-map->key-sequence
  ([m]
   (count-map->key-sequence m #{} []))
  ([m prev coll]
   (if (every? (fn [x] (= 0 x)) (vals m))
     coll
     (let [nxt (get-next-key m prev)]
       (count-map->key-sequence (update m nxt dec)
                                #{}
                                (conj coll nxt))))))

(defn- take-by-keyword-seq
  [m kw-seq]
  (if (empty? kw-seq)
    nil
    (let [curr-key (first kw-seq)]
      (cons (first (curr-key m))
            (take-by-keyword-seq (update m curr-key (partial drop 1))
                                 (rest kw-seq))))))

(defn weave-seqs
  [m]
  (let [count-map (clojure.algo.generic.functor/fmap count m)
        key-seq (count-map->key-sequence count-map)]
    (take-by-keyword-seq m key-seq)))

;; (let [a {:a (range 7) :b (range 70 72) :c (range 1111 1122)}]
;;   (weave-seqs a))

