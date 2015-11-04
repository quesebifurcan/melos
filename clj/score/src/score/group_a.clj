(ns score.group-a
  (:require [clojure.math.combinatorics :as combinatorics]
            [score.combinations :refer [unfold-parameters]]
            [melos.note :refer [make-note]]
            [melos
             [utils :as utils]]))

(defn wrap-all-in-vector
  [xs]
  (map #(conj [] %) xs))

(defn transpose-all
  [step forms]
  (clojure.walk/postwalk
   (fn [form]
     (cond (number? form)
           (+ form step)
           :else
           form))
   forms))

(defn upper
  []
  (letfn [(blueprint [{:keys [part-name
                              transposition
                              dur
                              drop-n]}]
            {:pitch (->> (range 10)
                         (transpose-all transposition)
                         (wrap-all-in-vector))
             :part [part-name]
             :fn utils/make-chord-from-pitch-vector-params
             :partition (partial utils/cyclic-partition [2])
             :merge-left? [false]
             :merge-right? [false]
             :drop-n drop-n
             :duration dur})]
    (->> {:part-name [:upper]
          :transposition [-1 0 2 3]
          :drop-n [0 1 2]
          :dur [[1/4] [2/4]]}
         (unfold-parameters)
         (map (comp utils/unfold-events blueprint)))))

(defn lower
  []
  (letfn [(blueprint [{:keys [part-name
                              transposition
                              drop-n
                              dur]}]
            {:pitch (->> (range 10)
                         (utils/transpose transposition)
                         (wrap-all-in-vector))
             :part [part-name]
             :fn utils/make-chord-from-pitch-vector-params
             :partition (partial utils/cyclic-partition [2])
             :drop-n drop-n
             :duration dur})]
    (->> {:part-name [:lower]
          :transposition [-7 -6]
          :drop-n [0 1 2]
          :dur [[1/4]]}
         (unfold-parameters)
         (map (comp utils/unfold-events blueprint)))))

(defn ped
  []
  (letfn [(blueprint [{:keys [part-name
                              transposition
                              drop-n
                              dur]}]
            {:pitch (->> (range 10)
                         (utils/transpose transposition)
                         (wrap-all-in-vector))
             :part [part-name]
             :fn utils/make-chord-from-pitch-vector-params
             :partition (partial utils/cyclic-partition [1])
             :drop-n drop-n
             :duration dur})]
    (->> {:part-name [:ped]
          :transposition [-20]
          :drop-n [0 1 2]
          :dur [[1/4]]}
         (unfold-parameters)
         (map (comp utils/unfold-events blueprint)))))

(def materials
  {:upper (upper)
   :lower (lower)
   :ped (ped)
   :melodic-indices [(take 20 (cycle [:upper :lower :ped]))]})
