(ns melos.scores.main
    (:require [schema.core :as s]
              [melos.tools.schemata :as schemata]
              [melos.tools.utils :refer [export-to-json]]
              [melos.scores.graphs.score-graph-1 :refer [compose-segment]]
              [melos.scores.tools :refer [unfold-segments
                                          compose-score]]
              [melos.scores.segments.segments-1 :refer [initial-score-segment
                                                        changes]]))

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
                 (compose-score (initial-score-segment)
                                (changes)
                                compose-segment)))
