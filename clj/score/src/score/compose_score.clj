(ns score.compose-score
  (:require [clojure.math.combinatorics :as combinatorics]
            [melos
             [graphs :as graphs]
             [schemas :as ms]
             [params :as params]
             [utils :as utils]]
            [melos.utils.cycle-params :refer [unfold-parameter-cycles]]
            [schema.core :as s]
            [score.combinations :as combinations]
            [score.materials
             [event-seqs :as event-seqs]
             [measures :as measures]
             [pairwise-mod :as pairwise]
             [stepwise-mod :as stepwise]]))

(defn upper-part
  [{:keys [transposition part-name]}]
  (->> (event-seqs/upper part-name transposition)
       (utils/unfold-events)))

(defn initial-state
  []
  {:melody-sources {:upper/a {:fn upper-part
                              :params {:transposition '[unfold [-10 10 0]]
                                       :part-name :upper}}
                    :lower/a {:fn upper-part
                              :params {:transposition -5
                                       :part-name :lower}}
                    :ped/a {:fn upper-part
                            :params {:transposition '[unfold [-15 -14 -13]]
                                     :part-name :ped}}}
   :diss-fn-params {:max-count '[unfold [10 20]]
                    :max-lingering 300
                    :diss-value '[unfold [[0 2 4 5]
                                          [0 1 2]]]}
   :mod-dur-patterns []
   :tempo 240
   :measures ['unfold [[measures/measure-4] [measures/measure-3]]]
   :last-event-extension 7/4
   :part-names [:upper :lower :ped]
   :melodic-indices {:fn (fn [{:keys [cnt xs]}]
                           (take cnt (cycle xs)))
                     :params {:xs '[unfold
                                    [[:upper/a :lower/a :upper/a :ped/a]
                                     [:upper/a :lower/a :ped/a]]]
                              :cnt '[unfold [10 21]]}}})

(defn compose
  []
  (let [filters #(sort-by (fn [x]
                            (apply + (map count (:merged-chord-seq x))))
                          %)]
    (->> (initial-state)
         (combinations/nested-map-product)
         (take 5)
         (map params/evaluate-nested-fns)
         (filters)
         (map graphs/compose-segment))))
