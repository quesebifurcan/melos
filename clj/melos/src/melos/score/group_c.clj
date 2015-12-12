(ns melos.score.group-c
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
            {:pitch (->> 
                         [[0] [0 1] [0 2]]
                         (transpose-all transposition))
             :part [part-name]
             :fn utils/make-chord-from-pitch-vector-params
             :partition (partial utils/cyclic-partition [3])
             :merge-left? [false]
             ;; :dissonance-contributor? [false true false false true true]
             :dissonance-contributor? [false false true]
             :duration (concat (repeat 8 1/4) [3/4] [3/4])
             :merge-right? [false]
             :drop-n drop-n})]
    (->> {:part-name [:upper]
          :pitches [[0 2 4 5 7 -3 -2 3 1 6 5]]
          :transposition [0]
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
                         (map (fn [x] [x]))
                         (transpose-all transposition))
             :part [part-name]
             :fn utils/make-chord-from-pitch-vector-params
             :partition (partial utils/cyclic-partition [1 1 1])
             :drop-n drop-n
             :duration dur})]
    (->> {:part-name [:lower]
          :pitches [[0 2 4 5 7 9 10 3 1 6 5]]
          :transposition [-5 -4 -3 -2]
          :drop-n (range 7)
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
                         (map (fn [x] [x]))
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
          :transposition [-15]
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
  (and (>= (count events) 3)
       (every? (partial score-utils/part-count-sufficient? 3) events)))

(def diss-params
  {:check (fn [events]
            (let [asdf (chord/dissonance-contributors events)]
            (<= (chord/scaled-dissonance-value (map :pitch asdf))
                (chord/scaled-dissonance-value [0 2 4 5]))))})

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

(defn post-process
  [events]
  (letfn [(filterfn [x]
            (->> x
                 (partition-by partition-events-fn)
                 (filter filter-events-fn)
                 (take 1)))]
  (->> events
       (mapcat filterfn)
       (utils/distinct-by score-utils/pitch-profile)
       (drop 70)
       (take 30)
       (sort-by (fn [x] 1))
       (partition 5)
       (assoc {} :segments))))

(def session-config
  {:persist-to "testing-c.edn"
   :params {
            :filter-fn (fn [x]
                         (->> x
                              (partition-by partition-events-fn)
                              (filter filter-events-fn)
                              (take 1)))
            :chord-seqs materials
            :initial-state-fn initial-state
            :post-process post-process
            }})
