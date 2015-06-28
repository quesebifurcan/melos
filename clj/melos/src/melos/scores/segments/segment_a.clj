(ns melos.scores.segments.segment-a
  (:require [schema.core :as s]
            [melos.tools.schemata :as ms]
            [melos.tools.cycle-params :refer [unfold-parameter-cycles]]
            [melos.tools.utils :as utils]
            [melos.scores.compose-segment :refer [compose-segment]]
            [melos.scores.graphs.score-graph :as score-graph]
            [melos.scores.materials.event-seqs.instruments :as instruments]
            [melos.scores.materials.measures :as measures]
            [melos.scores.materials.dissonance-fn-params :as dissonance-fn-params]
            [melos.scores.materials.dissonance-maps :as dissonance-maps]
            [melos.scores.ctrl-fns.pairwise :as pairwise]))

(s/defn initial-score-segment
  :- ms/ScoreSegment
  []
  (utils/make-score-segment {:melodic-indices (take 100 (cycle [:upper/a :lower/a :ped/a]))
                             :diss-fn-params dissonance-fn-params/default
                             :interval->diss-map dissonance-maps/default
                             :time-signatures [measures/measure-3]
                             :mod-dur-patterns []
                             :tempo 144
                             :part-names [:upper :lower :ped]
                             :melody-sources (atom (instruments/organ))}))

(s/defn changes
  :- [ms/PartialScoreSegment]
  []
  (unfold-parameter-cycles [{}] 1))

(defn compose
  []
  (compose-segment {:initial-state (initial-score-segment)
                    :changes (changes)
                    :graph score-graph/lazy-segment-graph}))

