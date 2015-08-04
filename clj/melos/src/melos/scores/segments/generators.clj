(ns melos.scores.segments.generators
  (:require [schema.core :as s]
            [melos.tools.schemata :as ms]
            [melos.tools.cycle-params :refer [unfold-parameter-cycles]]
            [melos.tools.utils :as utils]
            [melos.scores.compose-segment :refer [compose-segment]]
            [melos.scores.graphs.score-graph :as score-graph]
            [melos.scores.materials.event-seqs :as event-seqs]
            [melos.scores.materials.measures :as measures]
            [melos.scores.materials.dissonance-maps :as dissonance-maps]
            [melos.scores.ctrl-fns.stepwise :as stepwise]
            [melos.scores.segments.unfold-event-seq :as unfold-event-seq]
            [melos.scores.ctrl-fns.pairwise :as pairwise]))

(require '[melos.tools.rtm :as rtm]
         '[melos.tools.filter-parts :as filter-parts]
         '[melos.tools.selector-sequence :refer [collect-events-in-segment]]
         '[melos.tools.utils :as utils]
         '[melos.tools.cycle-params :as cycle-params]
         '[clojure.algo.generic.functor :as functor]
         '[clojure.math.combinatorics :as combinatorics])

(defn unfold-parameters
  [m]
  (map (fn [x] (zipmap (keys m) x))
       (apply combinatorics/cartesian-product (vals m))))

(defn calc-event-combination
  [state updates]
  (->> (unfold-event-seq/unfold-events state updates)
       (score-graph/lazy-segment-graph)
       (:result)))

(defn calc-event-combinations
  [state changes]
  (map (partial calc-event-combination state) changes))

(defn upper-part
  [{:keys [transposition part-name]}]
  (utils/unfold-events (event-seqs/upper part-name transposition)))

(defn compose-all
  [filters phrases]
  (->> (filters phrases)
       (map (fn [phrase] (-> (score-graph/lazy-rtm-graph {:tempo 240
                                                          :event-seq phrase
                                                          :measures [measures/measure-4]
                                                          :part-names [:upper :lower :ped]
                                                          :last-event-extension 7/4})
                             (:result))))))

(defn compose
  []
  (let [changes (unfold-parameters
                 {[:melody-sources :upper/a :params :transposition] [-10 10 0]
                  [:melodic-indices] [
                                      (take 21 (cycle [:upper/a :lower/a :ped/a]))
                                      (take 18 (cycle [:upper/a :lower/a :upper/a :ped/a]))
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
               :interval->diss-map dissonance-maps/default
               :time-signatures [measures/measure-4]
               :mod-dur-patterns []
               :tempo 240
               :part-names [:upper :lower :ped]
               :melodic-indices []}
        filters #(sort-by (fn [x] (apply + (map count x))) %)]
    (->> (calc-event-combinations state changes)
         (compose-all filters))))
