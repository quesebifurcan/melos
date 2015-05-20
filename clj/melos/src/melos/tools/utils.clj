(ns melos.tools.utils
  (:require [clojure.java.io :as io]
            [clojure.walk :as walk]
            [clojure.data.json :as json]))

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

(defn ratio->non-reduced-ratio-vector 
  "Since it is not possible to encode ratios as json, we need to
  convert all ratios to a non-reduced vector of numbers."
  [r]
  (let [mapping {1 [4 4]
                 1/2 [2 4]
                 6/4 [6 4]}]
    (if-let [result (get mapping r)]
      result
      [(numerator r) (denominator r)])))

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

(defn read-json-file
  "Read a json file and make sure that any key which is a string is
  transformed into a keyword."
  [f]
  (with-open [reader (clojure.java.io/reader f)]
    (->> (json/read reader)
         (walk/keywordize-keys))))

(defn write-json-to-file [path data]
  (spit path (json/write-str data)))

(defn export-to-json
  "Export a score. Replace all duration ratios with non-reduced ratio vectors."
  [path score]
  (->> score
       ;; Replace ratios with vector ratios for better json.
       (walk/postwalk (fn [form]
                        (if (get form :delta-dur)
                          (update-in form
                                     [:delta-dur]
                                     ratio->non-reduced-ratio-vector)
                          form)))
       (write-json-to-file path)))

(defn mapply
  [f & args]
  (apply f (apply concat (butlast args) (last args))))
