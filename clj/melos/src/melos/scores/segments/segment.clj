(ns melos.scores.segments.segment
  (:require [schema.core :as s]
            [melos.tools.schemata :as ms]
            [melos.tools.rtm :as rtm]
            [melos.tools.dissonance-calculator :refer [dissonance-map-default]]
            [melos.tools.cycle-params :refer [unfold-parameter-cycles]]
            [melos.scores.materials.event-seqs.organ :as organ]
            [melos.scores.materials.measures :as measures]
            [melos.scores.materials.part-seq :as part-seq]
            [melos.scores.materials.dissonance-fn-params :as dissonance-fn-params]
            [melos.scores.materials.dissonance-maps :as dissonance-maps]
            [melos.scores.ctrl-fns.pairwise :as pairwise]))

(defn initial-score-segment
  []
  {:part-seq (part-seq/retrieve :a 20)
   :diss-fn-params (dissonance-fn-params/retrieve :a)
   :interval->diss-map (dissonance-maps/retrieve :favor-dissonant)
   :part->event {:lower :lower/a, :upper :upper/a, :ped :ped/a}
   ;; TODO: pass in via score-graph.
   :time-signatures [measures/measure-3]
   :mod-dur-patterns [pairwise/dissonant-melody-movement-mod]
   :duration-scalar 1
   :tempo 144
   :part-names [:upper :lower :ped]
   :melody-sources (atom (organ/organ))
   :count 5})

(s/defn changes
  :- [ms/PartialScoreSegment]
  []
   (unfold-parameter-cycles
    [{:path [:count]
      :cycle [1]
      :values [30]}
     {:path [:diss-fn-params :diss-value]
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
     {:path [:duration-scalar]
      :cycle [1]
      :values [1]}
     {:path [:diss-fn-params :max-count]
      :cycle [1]
      :values [5 7 8]}
     {:path [:time-signatures]
      :cycle [1]
      :values [[measures/measure-3]]}
     ]
    10))

;; TODO: rhythms from event-count.
;; TODO: attach time-signatures.
;; TODO: chords (simultaneous events) -- group events with ids such that the dissonance calculator removes none or all of them.
;; TODO: validate top-level functions.

;; TODO: unexpected changes.
;; TODO: all time-signatures (DSL?)
;; TODO: Score presets: "chords", "hoketus", "slow" etc.
;; TODO: Tempo.

;; TODO: all-children-same-pitch? does not work with chords.

;; TODO: "Cascading" or short-circuiting modifications applied to durations?

;; TODO: other dissonant-maps, i.e. treating M3s as dissonances.
