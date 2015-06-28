(ns melos.scores.segments.segment
  (:require [schema.core :as s]
            [melos.tools.schemata :as ms]
            [melos.tools.rtm :as rtm]
            [melos.tools.dissonance-calculator :refer [dissonance-map-default]]
            [melos.tools.cycle-params :refer [unfold-parameter-cycles]]
            [melos.scores.compose-segment :refer [compose-segment]]
            [melos.scores.graphs.score-graph :as score-graph]
            [melos.scores.materials.event-seqs.instruments :as instruments]
            [melos.scores.materials.measures :as measures]
            [melos.scores.materials.part-seq :as part-seq]
            [melos.scores.materials.dissonance-fn-params :as dissonance-fn-params]
            [melos.scores.materials.dissonance-maps :as dissonance-maps]
            [melos.scores.ctrl-fns.pairwise :as pairwise]))

(defn initial-score-segment
  []
  {:melodic-indices (take (* 3 50)
                          (cycle [:upper/a :lower/a :ped/a]))
   :diss-fn-params (:a dissonance-fn-params/presets)
   :interval->diss-map (:favor-dissonant dissonance-maps/presets)
   :time-signatures [measures/measure-3]
   :mod-dur-patterns [pairwise/dissonant-melody-movement-mod]
   :tempo 144
   :part-names [:upper :lower :ped]
   :melody-sources (atom (instruments/organ))})

(s/defn changes
  :- [ms/PartialScoreSegment]
  []
  (unfold-parameter-cycles
   [{:path [:diss-fn-params :diss-value]
     :cycle [1]
     :values [[0 2 4]]}
    {:path [:interval->diss-map]
     :cycle [1]
     :values [dissonance-map-default]}
    {:path [:diss-fn-params :part-counts :lower]
     :cycle [1]
     :values [3]}
    {:path [:tempo]
     :cycle [1]
     :values [132 118 152]}
    {:path [:diss-fn-params :max-count]
     :cycle [1]
     :values [5 7 8]}
    {:path [:time-signatures]
     :cycle [1]
     :values [[measures/measure-3]]}
    ]
    1))

(defn compose
  []
  (compose-segment {:initial-state (initial-score-segment)
                    :changes (changes)
                    :graph score-graph/lazy-segment-graph}))

;; TODO: all time-signatures (DSL?)
;; TODO: Score presets: "chords", "hoketus", "slow" etc.
;; TODO: "Cascading" or short-circuiting modifications applied to durations?
