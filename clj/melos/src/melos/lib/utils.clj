(ns melos.lib.utils
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.walk :as walk]
            [clojure.math.combinatorics :as combinatorics]

            [clojure.algo.generic.functor :as functor]
            [melos.lib
             [note :refer [make-note]]
             [schemas :as ms]]
            [schema.core :as s]))

(defn abs [n] (max n (- n)))

(defn sum [args] (apply + args))

(defn rotate
  ([s] (rotate 1 s))
  ([n s] (lazy-cat (drop n s) (take n s))))

(letfn [(merge-in* [a b]
          (if (map? a)
            (merge-with merge-in* a b)
            b))]
  (defn merge-in
    [& args]
    (reduce merge-in* nil args)))

(defn- triangular*
  ([] (triangular* 0 1))
  ([sum n]
   (let [new-sum (+ sum n)]
     (cons new-sum (lazy-seq (triangular* new-sum (inc n)))))))

(def triangular (triangular*))

(defn triangular-n
  "Get the nth triangular number."
  [n]
  (last (take n triangular)))

(defn export-to-json [path data]
  (spit path (json/write-str data)))

(defn segment-melody
  [xs]
  (->> xs
       (partition-by #(= % :end))
       (take-nth 2)))

(defn make-chord-from-pitch-vector-params
  [{:keys [pitch] :as m}]
  (let [group (gensym "G__")]
    (map (fn [p]
           (make-note (merge {:pitch p :group group}
                             (dissoc m :pitch))))
         pitch)))

(defn cyclic-partition
  [splits xs]
  (cons (take (first splits) xs)
        (lazy-seq (cyclic-partition (rotate splits)
                                    (drop (first splits) xs)))))

(defn cyclic-repeats
  [repeats xs]
  (if (seq xs)
    (concat (repeat (first repeats) (first xs))
            (lazy-seq (cyclic-repeats (rotate repeats)
                                      (rotate xs))))))

(defn transpose
  [step coll]
  (map (partial + step) coll))

(defn transpose-all
  [step forms]
  (clojure.walk/postwalk
   (fn [form]
     (cond (number? form)
           (+ form step)
           :else
           form))
   forms))

(defn distinct-by
  "Returns a lazy sequence of the elements of coll, removing any elements that
  return duplicate values when passed to a function f."
  [f coll]
  (let [step (fn step [xs seen]
               (lazy-seq
                ((fn [[x :as xs] seen]
                   (when-let [s (seq xs)]
                     (let [fx (f x)]
                       (if (contains? seen fx)
                         (recur (rest s) seen)
                         (cons x (step (rest s) (conj seen fx)))))))
                 xs seen)))]
    (step coll #{})))

;; Rhythm Trees

(defn ratio-to-non-reduced-ratio-vector
  ;; "Since it is not possible to encode ratios as json, we need to
  ;; convert all ratios to a non-reduced vector of numbers."
  [r]
  (let [r (clojure.lang.Numbers/toRatio (rationalize r))]
    ((juxt numerator denominator) r)))

;; (ratio-to-non-reduced-ratio-vector 7/4)
;; => [7 4]
;; (ratio-to-non-reduced-ratio-vector 4/4)
;; => [1 1]

(defn partition-groups
  [f curr coll l]
  (if (empty? l)
    (if (empty? curr)
      coll
      (concat coll [curr]))
    (let [nxt (first l)]
      (if (f nxt)
        (partition-groups f
                          []
                          (concat coll
                                  [(concat curr
                                           [nxt])])
                          (rest l))
        (partition-groups f
                          (concat curr [nxt])
                          coll
                          (rest l))))))

(defn unfold-parameters
  [m]
  (->> m
       (iterate (partial functor/fmap rotate))
       (map (partial functor/fmap first))))

;; (take 5 (unfold-parameters {:a [1 2] :b [3 4 5 6]}))
;; => ({:a 1, :b 3} {:a 2, :b 4} {:a 1, :b 5} {:a 2, :b 6} {:a 1, :b 3})

(defn apply-slope
  ([cnt start]
   (repeat cnt start))
  ([cnt start end]
   (conj (vec (repeat (dec cnt) start))
         end))
  ([cnt start mid end]
   (let [mid_pos (int (Math/floor (/ cnt 2)))
         end_pos (dec cnt)]
     (map (fn [x]
            (cond (< x mid_pos)
                  start
                  (= x end_pos)
                  end
                  :else
                  mid))
          (range cnt)))))

;; (apply-slope 5 1)
;; => (1 1 1 1 1)
;; (apply-slope 5 1 4)
;; => [1 1 1 1 4]
;; (apply-slope 10 1 4 9)
;; (1 1 1 1 1 4 4 4 4 9)
