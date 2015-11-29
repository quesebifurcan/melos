(ns melos.score.group-d
  (:require [clojure.math.combinatorics :as combinatorics]
            [melos.lib
             [note :refer [make-note]]
             [chord :as chord]
             [utils :as utils]]
            [melos.score
             [combinations :refer [unfold-parameters]]
             [score-utils :as score-utils]]
            [melos.score.materials
             [measures :as measures]]))

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
            {:pitch (->> (concat (range -3 21) [20])
                         (map (fn [x] [2 x]))
                         (transpose-all transposition))
             :part [part-name]
             :fn utils/make-chord-from-pitch-vector-params
             :partition (partial utils/cyclic-partition [2 3 1])
             :merge-left? [false]
             :duration (concat (repeat 8 1/4) [3/4] [3/4])
             :merge-right? [false]
             :drop-n drop-n})]
    (->> {:part-name [:upper]
          :pitches [[0 2 4 5 7 -3 -2 3 1 6 5]]
          :transposition [10]
          :drop-n (range 4)
          :dur [[1/4 2/4 3/4 6/4]]}
         (unfold-parameters)
         (map (comp utils/unfold-events blueprint)))))

(defn lower
  []
  (letfn [(blueprint [{:keys [pitches
                              part-name
                              transposition
                              drop-n
                              dur]}]
            {:pitch (->> (range 8)
                         (map (fn [x] [4 x]))
                         (transpose-all transposition))
             :part [part-name]
             :fn utils/make-chord-from-pitch-vector-params
             :partition (partial utils/cyclic-partition [3 1 1])
             :drop-n drop-n
             :duration dur})]
    (->> {:part-name [:lower]
          :pitches [[0 2 4 5 7 9 10 3 1 6 5]]
          :transposition [5]
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
            {:pitch (->> (range 6)
                         (wrap-all-in-vector)
                         (transpose-all transposition))
             :part [part-name]
             :fn utils/make-chord-from-pitch-vector-params
             :partition (partial utils/cyclic-partition [1])
             :drop-n drop-n
             :duration dur})]
    (->> {:part-name [:ped]
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
                    ]
          :transposition [-5]
          :drop-n (range 5)
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

(def partition-events-fn
  (partial score-utils/part-count-sufficient? 3))

(defn filter-events-fn
  [events]
  (and (>= (count events) 6)
       (every? (partial score-utils/part-count-sufficient? 3) events)))

(def diss-params
  {:check (fn [events]
            (<= (chord/scaled-dissonance-value (map :pitch events))
                (chord/scaled-dissonance-value [0 1 2])))})

(def initial-state
  {:diss-fn-params {:max-count 100
                    :max-lingering 300
                    ;; TODO: pass on diss-value
                    :diss-params diss-params}
   :tempo 200
   ;; :measures is not used -- it is hardcoded in compose-score/compose
   :measures [measures/measure-3]
   :pre []
   :post []
   :part-names [:upper :lower :ped]})

(def session-config
  {:persist-to "testing-d.edn"
   :params {:filter-fn (fn [x]
                         (->> x
                              (partition-by partition-events-fn)
                              (filter filter-events-fn)
                              (take 1)))
            :distinct-by-fn score-utils/pitch-profile
            :chord-seqs materials
            :partition-count 2
            ;; :segmentation-fn (fn [x]
            :initial-state-fn initial-state
            :sort-by-fn (fn [x] 1) }})

