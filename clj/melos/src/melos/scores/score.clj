(ns melos.scores.score
    (:require [schema.core :as s]
              [melos.tools.schemata :as schemata]
              [melos.scores.materials.event-seqs :refer [organ]]
              [melos.tools.delay-lines :refer [handle-dissonance]]
              [melos.tools.rtm :as rtm]
              [melos.tools.utils :refer [merge-in
                                         export-to-json]]
              [melos.scores.materials.measures :as measures]
              [melos.tools.modify-durations :as mod-dur]
              [melos.tools.cycle-params :refer [unfold-parameter-cycles]]))

;; Functions for controlling rhythmic aspects of the score.

(declare rtm-fn)
(declare time-signature-fn)

(def diss-fn-params
  "The main function we are going to use to control the
  treatment of dissonances in this piece."
  {:max-count 8
   :part-counts {:upper 1
                 :lower 2
                 :ped 1}
   :max-lingering 5
   :diss-value 1.6})

(def part-seq
  "A seq of part names, in this particular case corresponding to three
  organ manuals."
  [:lower :upper :lower :upper :ped])

(require '[melos.tools.dissonance-calculator :refer
           [dissonance-map-default dissonance-map-2]])

(defn initial-score-segment
  []
  {:part-seq part-seq
   :diss-fn-params diss-fn-params
   :interval->diss-map dissonance-map-default
   :part->event {:lower :a, :upper :a, :ped :a}
   ;; TODO: pass in via score-graph.
   :time-signatures [measures/measure-3]
   :mod-dur-patterns [mod-dur/dissonant-melody-movement-mod]
   :duration-scalar 1
   :part-names [:upper :lower :ped]
   :melody-sources (atom (organ))
   :count 200})

(s/defn changes
  :- [schemata/PartialScoreSegment]
  []
   (unfold-parameter-cycles
    [{:path [:count]
      :cycle [1]
      :values [10]}
     {:path [:diss-fn-params :diss-value]
      :cycle [1]
      :values [1.6]}
     {:path [:interval->diss-map]
      :cycle [1]
      :values [dissonance-map-default]}
     {:path [:diss-fn-params :part-counts :lower]
      :cycle [1]
      :values [1]}
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

;; FORM -- parameters.
;; Melody-variables: contour (register), expansion-type (applied to melody), repetition-type,
;; Section-variables: duration-scalar, dissonance-map, dissonance-fn,
;; melody-sources, count, time-signatures, dissonance-duration modifications

;; Using graphs introduces a clear separation between:
;; 1. Values that are *given*, i.e. the compositional interface, and
;; 2. Values that are *calculated*, i.e. derived from the compositional interface.
;; Probably this makes it possible to remove all boilerplate code
;; where values of the segment-map have to be "assoced back" onto the
;; segment. Which would be a wonderful thing.

;; ScoreSegment -- input to (segment-graph)
;; PartialScoreSegment -- output of (changes); input to (make-score)

;; Persist as json.
;; (export-to-json "/Users/fred/Desktop/score.json"
;;                 (compose-score))
