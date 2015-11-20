(ns score.group-c
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
  (letfn [(blueprint [{:keys [pitches
                              part-name
                              transposition
                              dur
                              drop-n]}]
            ;; {:pitch (->> (range 10)
            ;;              (transpose-all transposition)
            ;;              (wrap-all-in-vector))
            ;; {:pitch (->> [-3 -2 5 2 3 10 3 2 5 -2]
            ;; {:pitch (->> (range -3 20)
            ;; {:pitch (->> [0 12 14 2 3 15 13 1]
            ;; {:pitch (->> [[2] [2 4] [2 4 9] [9] [-3 9] [-3] [-3 2] [-3 2 4]]
            {:pitch (->> (range 10)
                         ;; (wrap-all-in-vector)
                         (map (fn [x] [4 x]))
                         (transpose-all transposition))
             :part [part-name]
             :fn utils/make-chord-from-pitch-vector-params
             :partition (partial utils/cyclic-partition [2 3 1])
             :merge-left? [false]
             :merge-right? [false]
             :drop-n drop-n
             :duration dur})]
    (->> {:part-name [:upper]
          :pitches [[0 2 4 5 7 -3 -2 3 1 6 5]]
          :transposition [0]
          :drop-n (range 6)
          :dur [[1/4]]}
         (unfold-parameters)
         (map (comp utils/unfold-events blueprint)))))

(defn lower
  []
  (letfn [(blueprint [{:keys [pitches
                              part-name
                              transposition
                              drop-n
                              dur]}]
            ;; {:pitch (->> [-5 0 2 7 2 0 -5 0 2 7 12 14 12 7 2 0]
            ;; {:pitch (->> (range -3 7)
            ;; {:pitch (->> [0 7 12 14 2 -5 7 5 4 3 10 3]
            ;; {:pitch (->> (concat (range -3 8) (range 8 -3 -1))
            ;; {:pitch (->> [[0] [0 -1] [0 -1 -3] [-3 0] [-1]]
            ;; {:pitch (->> pitches
            ;;              (transpose-all transposition)
            ;;              (wrap-all-in-vector)
            ;;              )
            {:pitch (->> (range 8)
                         ;; (wrap-all-in-vector)
                         (map (fn [x] [4 x]))
                         (transpose-all transposition))
             :part [part-name]
             :fn utils/make-chord-from-pitch-vector-params
             :partition (partial utils/cyclic-partition [3 1 1])
             :drop-n drop-n
             :duration dur})]
    (->> {:part-name [:lower]
          :pitches [[0 2 4 5 7 9 10 3 1 6 5]]
          :transposition [-5]
          :drop-n (range 3)
          :dur [[1/4]]}
         (unfold-parameters)
         (map (comp utils/unfold-events blueprint)))))

(defn ped
  []
  (letfn [(blueprint [{:keys [pitches
                              part-name
                              transposition
                              drop-n
                              dur]}]
            ;; {:pitch (->> [3 2 0 -2]
            {:pitch (->> (range 6)
                         (wrap-all-in-vector)
                         (transpose-all transposition))
             :part [part-name]
             :fn utils/make-chord-from-pitch-vector-params
             :partition (partial utils/cyclic-partition [1])
             :drop-n drop-n
             :duration dur})]
    (->> {:part-name [:ped]
          ;; :pitches [[0 2 4 5 7 9 10 3 1 6 5]]
          :pitches [
                    [
                     [0] [0 -2] 
                     [-2] [-2 0]
                     [0] [0 2]
                     [2] [2 0]
                     ]
                    [
                     [0] [0 -2] 
                     [-2] [-2 -3]
                     [-3] [-2 -3]
                     [-2] [-2 0]
                     ]
                    ;; (transpose-all -1 [[0] [0 -2] [-2] [-2 0]])
                    ]
          :transposition [-15]
          :drop-n (range 10)
          :dur [[1/4]]}
         (unfold-parameters)
         (map (comp utils/unfold-events blueprint)))))

(def materials
  {:upper (upper)
   :lower (lower)
   :ped (ped)
   :melodic-indices [
                     ;; max number of events:
                     (take 10 (cycle [:upper :lower :ped]))
                     (take 10 (cycle [:upper :lower :ped :lower :ped]))
                     ;; (take 20 (cycle [:upper :lower :ped :lower :ped :lower :ped :lower]))
                     ]})

