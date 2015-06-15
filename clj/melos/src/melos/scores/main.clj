(ns melos.scores.main
    (:require [schema.core :as s]
              [melos.tools.schemata :as schemata]
              [melos.tools.rtm :as rtm]
              [melos.tools.utils :refer [merge-in
                                         export-to-json]]
              [melos.scores.graphs.score-graph-1 :refer [compose-segment]]
              [melos.scores.utils.unfold-segments :refer [unfold-segments]]
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

(defn compose-score
  "Compose a score:

  - Merge all changes into a seq of maps.
  - Collect all melodic events and split the resulting data structure into
  *segments*.
  - Compose each segment.
  "
  [initial-score-segment changes]
  (->> (unfold-segments initial-score-segment changes)
       (map compose-segment)
       (map :result)
       (rtm/merge-all-tied)))

(time
 (export-to-json "/Users/fred/Desktop/score.json"
                 (compose-score (initial-score-segment)
                                (changes))))
