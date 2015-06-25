(ns melos.main
    (:require [schema.core :as s]
              [melos.tools.schemata :as schemata]
              [melos.tools.utils :refer [export-to-json]]
              [melos.scores.graphs.score-graph :as score-graph]
              [melos.scores.segments.segment-tools :as score-tools]
              [melos.scores.segments.segment :as segment]))

;; ## Pretty-printing

(s/set-fn-validation! true)
(require '[clojure.pprint])

;; Suppress printing of lazy-seqs.
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

(time
 (export-to-json "/Users/fred/Desktop/score.json"
                 (score-tools/compose-score (segment/initial-score-segment)
                                            (segment/changes)
                                            score-graph/compose-segment)))
