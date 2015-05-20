(ns scores.score
    (:require [schema.core :as s]
              [melos.tools.schemata :as schemata]
              [scores.event-seqs :refer [organ]]
              [melos.tools.delay-lines :refer [handle-dissonance]]
              [melos.tools.rtm :as rtm]
              [melos.tools.utils :refer [export-to-json]]
              [melos.tools.core :refer [make-score
                                     ;; segment->parts
                                     compose-segment]]))

;; ## Pretty-printing

(require '[clojure.pprint])

(set! *print-length* false)

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
                      :max-lingering 10
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
   :count 80})

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

;; TODO: disallow extensions.
;; TODO: unexpected changes.
;; TODO: make merge, allow-extension, diss-fn optional.

;; TODO: "Cascading" or short-circuiting modifications applied to durations?

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
