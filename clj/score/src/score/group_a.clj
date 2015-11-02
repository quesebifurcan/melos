(ns score.group-a
  (:require [clojure.math.combinatorics :as combinatorics]
            [score.combinations :refer [unfold-parameters]]
            [melos.note :refer [make-note]]
            [melos
             [utils :as utils]]))

(defn transpose-all-numbers
  [step forms]
  (clojure.walk/postwalk
   (fn [form]
     (cond (number? form)
           (+ form step)
           :else
           form))
   forms))

(defn upper
  [{:keys [part-name transposition dur]}]
  {:pitch (transpose-all-numbers transposition [
           [0] [1 8] [2 8] [7] [8] [9] [1] [2] [3] [-7] [-6] [-5] [-4]
           ])
   :part [part-name]
   :fn utils/make-chord-from-pitch-vector-params
   :partition (partial utils/cyclic-partition [3 3 3 4])
   :merge-left? [false]
   :merge-right? [false]
   :duration [dur]})

(defn diatonic-ped
  [pitches part-name transposition]
  {:pitch (->> pitches
               (utils/transpose transposition)
               (map (fn [x] [x])))
               ;; (make-overlaps))
   :part [part-name]
   :fn utils/make-chord-from-pitch-vector-params
   :partition (partial utils/cyclic-partition [2])
   ;; :max-lingering [200]
   ;; :merge-left? [true]
   ;; :merge-right? [true]
   :duration [1/4]})

(defn diatonic-ped-2
  [part-name transposition]
  {:pitch (->> (range -4 8)
               (utils/transpose transposition)
               (map (fn [x] [x])))
   :part [part-name]
   :fn utils/make-chord-from-pitch-vector-params
   :partition (partial utils/cyclic-partition [1])
   :max-part-count [1]
   ;; :merge-left? [true]
   ;; :merge-right? [true]
   :duration [1/4]})

(def materials
  {
   :upper (->> {:part-name [:upper]
                :transposition [-1 0 2 3]
                :dur [1/4]}
               (unfold-parameters)
               (map (comp utils/unfold-events upper)))

   :lower (map (fn [offset] (drop offset
                                  (utils/unfold-events (diatonic-ped (range 10) :lower -7))))
                                  (range 4))

   :ped (map (fn [offset] (drop offset
                                (utils/unfold-events (diatonic-ped-2 :ped -20))))
             (range 4))

   :melodic-indices [(take 20 (cycle [:upper :lower :ped]))]})
