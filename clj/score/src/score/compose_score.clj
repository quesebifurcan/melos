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

(defn update-score-state
  [initial-state updates]
  (->> (utils/update-state initial-state updates)
       (params/evaluate-nested-fns)))

(defn upper-part
  [{:keys [transposition part-name]}]
  (->> (event-seqs/upper part-name transposition)
       (utils/unfold-events)))

(defn compose-all
  [filters phrases]
  (->> (filters phrases)
       (map graphs/compose-segment)))

(defn compose
  []
  (let [changes (combinations/unfold-parameters
                 {[:melody-sources :upper/a :params :transposition] [-10 10 0]
                  [:melodic-indices] [
                                      {:fn (fn [x] (take 21 (cycle x)))
                                       :params [:upper/a :lower/a :ped/a]}
                                      {:fn (fn [x] (take 10 (cycle x)))
                                       :params [:upper/a :lower/a :upper/a :ped/a]}
                                      ]
                  [:diss-fn-params :diss-value] [[0 2 4 5] [0 1 2]]
                  })
        state {:melody-sources {
                                :upper/a {:fn upper-part
                                          :params {:transposition -3
                                                   :part-name :upper}}
                                :lower/a {:fn upper-part
                                          :params {:transposition -5
                                                   :part-name :lower}}
                                :ped/a {:fn upper-part
                                        :params {:transposition -15
                                                 :part-name :ped}}
                                }
               :diss-fn-params {:max-count 10
                                :max-lingering 300
                                :diss-value [0 2 4 5]}
               :time-signatures [measures/measure-4]
               :measures [measures/measure-4]
               :mod-dur-patterns []
               :tempo 240
               :last-event-extension 7/4
               :part-names [:upper :lower :ped]
               :melodic-indices []}
        filters #(sort-by (fn [x]
                            (apply + (map count (:merged-chord-seq x))))
                          %)]
    (->> (map (fn [change-set] (update-score-state state change-set))
              changes)
         (compose-all filters))))
