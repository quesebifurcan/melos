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
            [melos.scores.ctrl-fns.pairwise :as pairwise]))

(s/defn initial-score-segment
  :- ms/ScoreSegment
  []
  (utils/make-score-segment {:melodic-indices (take 20 (cycle [
                                                               :upper/a
                                                               :lower/a
                                                               :ped/a
                                               ]))
                             :diss-fn-params {:max-count 10
                                              :max-lingering 300
                                              :diss-value [0 2 4 5]}
                             :interval->diss-map dissonance-maps/default
                             :time-signatures [measures/measure-4]
                             ;; :mod-dur-patterns [pairwise/sustain-dissonant-melody]
                             ;; :mod-dur-patterns [stepwise/remove-dissonant-vertical-moments]
                             ;; :mod-dur-patterns [stepwise/dissonance->durations-mapping]
                             ;; :mod-dur-patterns [pairwise/sustain-parts-count-drops]
                             ;; :mod-dur-patterns [stepwise/apply-melodic-pitch-class-mapping]
                             ;; :mod-dur-patterns [stepwise/apply-durations]
                             :mod-dur-patterns [stepwise/sort-continuous]
                             :tempo 240
                             :part-names [:upper :lower :ped]
                             :melody-sources (atom (event-seqs/organ))}))

(defn melodic-indices
  []
  (let [cnts [1000 4000 4000 4000]]
    (map (fn [cnt pattern]
           ;; (take cnt (cycle [
           ;;                   ;; :upper/a :lower/a :upper/a :ped/a :ped/a :lower/a
           ;;                   ;; :upper/a :lower/a :upper/a :ped/a
           ;;                   ;; :upper/a :lower/a :ped/a
           ;;                   ;; :upper/a :lower/a :ped/a :ped/a
           ;;                   ;; :upper/a :lower/a
           ;;                   ;; :upper/a :lower/a
           ;;                   ;; :upper/a :lower/a
           ;;                   ])))
           (take cnt (cycle pattern)))
         cnts
         [
          [:upper/a :lower/a :ped/a]
          [:upper/a :lower/a :upper/a :ped/a]
          [:upper/a :lower/a :upper/a :lower/a :ped/a]
          [:upper/a :lower/a :upper/a :ped/a :lower/a :ped/a]
          ])))

(s/defn changes
  :- [ms/PartialScoreSegment]
  []
  (let [tempo-measure-link [2 2]]
    (unfold-parameter-cycles
     [{:values (melodic-indices)
       :path [:melodic-indices]
       :cycle [1]}
      {:values [[measures/measure-4] [measures/measure-3]]
       :path [:time-signatures]
       :cycle tempo-measure-link}
      {:values [240]
       :path [:tempo]
       :cycle tempo-measure-link}]
     1)))

(defn compose
  []
  (compose-segment {:initial-state (initial-score-segment)
                    :changes (changes)
                    :graph score-graph/lazy-segment-graph}))

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

(defn evaluate-nested-fns
  [state]
  (clojure.walk/postwalk
   (fn [form]
     (if (and (map? form)
              (contains? form :fn)
              (contains? form :params))
       ((:fn form) (:params form))
       form))
   state))

(defn evaluate-once
  [state changes]
  (let [state (reduce (fn [m [k v]]
                        (update-in m k (fn [_] v)))
                      state
                      changes)]
    (->> state
         (evaluate-nested-fns)
         (score-graph/lazy-segment-graph)
         (:merged-horizontally)
         )))

(defn calc-event-combinations
  [state changes]
  (map (partial evaluate-once state)
       changes))

(defn apply-rhythm
  [last-event-extension tempo measures events]
  (->> events
       (rtm/extend-last last-event-extension)
       (rtm/make-r-tree measures)
       ((fn [rhythmic-tree]
          {:tempo tempo
           :parts (->> (map
                        (fn [part-name]
                          {:part-name part-name
                           :events (filter-parts/split-out-part rhythmic-tree part-name)})
                        [:upper :lower :ped])
                       (rtm/merge-all-tied))}))))

(defn upper-part
  [{:keys [transposition part-name]}]
  (utils/unfold-events (event-seqs/upper part-name transposition)))

(let [changes (unfold-parameters
               {[:melody-sources :upper/a :params :transposition] [-10 10 0]})
      state {:melody-sources {:upper/a {:fn upper-part
                                        :params {:transposition -3
                                                 :part-name :upper}}}
             :diss-fn-params {:max-count 10
                              :max-lingering 300
                              :diss-value [0 2 4 5]}
             :interval->diss-map dissonance-maps/default
             :time-signatures [measures/measure-4]
             :mod-dur-patterns []
             :tempo 240
             :part-names [:upper :lower :ped]
             :melodic-indices (take 21 (cycle [:upper/a]))}
      events (calc-event-combinations state changes)]
  (->> events
       (map (partial apply-rhythm 7/4 240 [measures/measure-4]))
       (utils/export-to-json
        "/Users/fred/Desktop/score.json")
       ))
