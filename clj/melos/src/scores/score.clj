(ns scores.score
    (:require [schema.core :as s]
              [melos.tools.schemata :as schemata]
              [scores.event-seqs :refer [organ]]
              [melos.tools.delay-lines :refer [handle-dissonance]]
              [melos.tools.rtm :as rtm]
              [melos.tools.utils :refer [export-to-json
                                         merge-in]]
              [melos.tools.score-graph :refer [
                                     ;; segment->parts
                                     compose-segment]]))

;; ## Pretty-printing

(require '[clojure.pprint])

(defmethod print-method
  clojure.lang.Atom
  [x ^java.io.Writer w]
  (.write w (str "#<" x ">")))

(defn custom-print-atom [x]
  (clojure.pprint/with-pprint-dispatch
    print
    (print x)))

(. clojure.pprint/simple-dispatch
   addMethod
   clojure.lang.Atom
   custom-print-atom)

;; ## Score modelling

(s/defn ^:always-validate make-score
  ;; "Deep-merge a seq of maps with an initial score state. See
  ;; src/scores.score.clj for more details."
  :- [schemata/ScoreSegment]
  [init :- schemata/ScoreSegment
   changes :- [schemata/PartialScoreSegment]]
  (reductions merge-in init changes))

;; Functions for controlling rhythmic aspects of the score.
(declare rtm-fn)
(declare time-signature-fn)

(def diss-fn
  "The main function we are going to use to control the
  treatment of dissonances in this piece."
  (handle-dissonance {:max-count 4
                      :part-count 5
                      :part-counts {:upper 1
                                    :lower 1
                                    :ped 1}
                      :max-lingering 5
                      :diss-value 1.6}))

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
   :diss-fn diss-fn
   :part->event {:lower :a, :upper :a, :ped :a}
   :time-signatures [4/4]
   :part-names [:upper :lower :ped]
   :time-signature-fn time-signature-fn
   :rtm-fn rtm-fn
   :melody-sources (atom (organ))
   :count 200})

(s/defn ^:always-validate changes
  :- [schemata/PartialScoreSegment]
  []
  "Each map represents a musical section. Only the *changes* (in
  relationship to the previous section) are notated. In other words:
  every parameter which is not explicitly changed will have the same
  value as in the previous section."
  [
   ;; {:count 20 :part->event {:ped :b}}
   ;; {:count 30 :part-seq [:ped :lower :lower :upper :upper :ped]}
   ])

(defn compose-partial-score
  []
  (->> (changes)
       (make-score (initial-score-segment))
       (map compose-segment)))

(defn compose-score
  "Compose a score:

  - Merge all changes into a seq of maps.
  - Collect all melodic events and split the resulting data structure into
  *segments*.
  - Compose each segment.
  "
  []
  (->> (changes)
       (make-score (initial-score-segment))
       (map compose-segment)
       (map :parts-tree)))

(time
 (export-to-json "/Users/fred/Desktop/score.json"
                 (-> (compose-score)
                     (rtm/update-children))))

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
