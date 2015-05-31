(ns scores.score
    (:require [schema.core :as s]
              [melos.tools.schemata :as schemata]
              [scores.event-seqs :refer [organ]]
              [melos.tools.delay-lines :refer [handle-dissonance]]
              [melos.tools.rtm :as rtm]
              [melos.tools.utils :refer [merge-in
                                         export-to-json]]
              [melos.tools.measures :as measures]
              [scores.main :refer [make-score
                                   compose-score]]
              [melos.tools.cycle-params :refer [unfold-parameter-cycles]]))

;; Functions for controlling rhythmic aspects of the score.

(declare rtm-fn)
(declare time-signature-fn)

(def diss-fn-params
  "The main function we are going to use to control the
  treatment of dissonances in this piece."
  {:max-count 8
   :part-counts {:upper 1
                 :lower 1
                 :ped 1}
   :max-lingering 5
   :diss-value 1.6})

(def part-seq
  "A seq of part names, in this particular case corresponding to three
  organ manuals."
  [:lower :upper :upper :upper :lower :upper :upper :upper :ped])

(defn initial-score-segment
  "This map describes the first section of the piece.

  - :part-seq -- in which order do the musical parts enter?
  - :diss-fn -- how control dissonance / simulate musical counterpoint?
  - :part->event -- with which musical materials are the parts currently related?
  - :time-signatures -- which time signatures are allowed in this section?
  - :time-signature-fn -- which function is used to match the available
    time signatures to the music?
  - :rtm-fn -- which function will we use to articulate rhythms?
  - :count -- how many events in this segment?
  "
  []
  {:part-seq part-seq
   :diss-fn-params diss-fn-params
   :part->event {:lower :a, :upper :a, :ped :a}
   ;; TODO: pass in via score-graph.
   :time-signatures [measures/measure-2]
   :duration-scalar 1
   :part-names [:upper :lower :ped]
   :melody-sources (atom (organ))
   :count 200})

(s/defn changes
  :- [schemata/PartialScoreSegment]
  []
  "Each map represents a musical section. Only the *changes* (in
  relationship to the previous section) are notated. In other words:
  every parameter which is not explicitly changed will have the same
  value as in the previous section."

  (unfold-parameter-cycles
   [{:path [:count]
     :cycle [1]
     :values [10]}
    {:path [:diss-fn-params :diss-value]
     :cycle [1]
     :values [1.6]}
    {:path [:diss-fn-params :part-counts :lower]
     :cycle [1]
     :values [1]}
    {:path [:duration-scalar]
     :cycle [1]
     :values [1]}
    {:path [:diss-fn-params :max-count]
     :cycle [1]
     :values [5 7 8]}]
   5))

(time
 (export-to-json "/Users/fred/Desktop/score.json"
                 (compose-score (initial-score-segment)
                                (changes))))

;; TODO: rhythms from event-count.
;; TODO: attach time-signatures.
;; TODO: chords (simultaneous events) -- group events with ids such that the dissonance calculator removes none or all of them.
;; TODO: validate top-level functions.

;; TODO: unexpected changes.
;; TODO: all time-signatures (DSL?)
;; TODO: Score presets: "chords", "hoketus", "slow" etc.
;; TODO: Tempo.

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
