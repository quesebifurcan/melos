(ns melos.scores.segments.segment-a
  (:require [schema.core :as s]
            [melos.tools.schemata :as ms]
            [melos.tools.cycle-params :refer [unfold-parameter-cycles]]
            [melos.tools.utils :as utils]
            [melos.scores.compose-segment :refer [compose-segment]]
            [melos.scores.graphs.score-graph :as score-graph]
            [melos.scores.materials.event-seqs :as event-seqs]
            [melos.scores.materials.measures :as measures]
            [melos.scores.materials.dissonance-maps :as dissonance-maps]
            [melos.scores.ctrl-fns.pairwise :as pairwise]))

(s/defn initial-score-segment
  :- ms/ScoreSegment
  []
  (utils/make-score-segment {:melodic-indices (take 25 (cycle [
                                               ;; :lower/a :upper/a :ped/a
                                               ;; :upper/a :lower/a :ped/a
                                                               :group/a
                                               ]))
                             :diss-fn-params {:max-count 10
                                              :max-lingering 300
                                              :diss-value [0 1 2]}
                             :interval->diss-map dissonance-maps/default
                             :time-signatures [measures/measure-4]
                             :mod-dur-patterns [pairwise/sustain-dissonant-melody]
                             ;; :mod-dur-patterns []
                             :tempo 240
                             :part-names [:upper :lower :ped]
                             :melody-sources (atom (event-seqs/organ))}))

(defn melodic-indices
  []
  (let [cnts [200]]
    (map (fn [cnt]
           (take cnt (cycle [:upper/a :lower/a :ped/a])))
         cnts)))

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
