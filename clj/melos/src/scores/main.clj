(ns scores.main
    (:require [schema.core :as s]
              [melos.tools.schemata :as schemata]
              [melos.tools.rtm :as rtm]
              [melos.tools.utils :refer [merge-in]]
              [scores.score-graph :refer [compose-segment]]))

;; ## Pretty-printing

(s/set-fn-validation! true)
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

(s/defn make-score
  ;; "Deep-merge a seq of maps with an initial score state. See
  ;; src/scores.score.clj for more details."
  :- [schemata/ScoreSegment]
  [init :- schemata/ScoreSegment
   changes :- [schemata/PartialScoreSegment]]
  (->> (reductions merge-in init changes)
       (rest)))

(defn compose-score
  "Compose a score:

  - Merge all changes into a seq of maps.
  - Collect all melodic events and split the resulting data structure into
  *segments*.
  - Compose each segment.
  "
  [initial-score-segment changes]
  (->> (make-score initial-score-segment changes)
       (map compose-segment)
       (map :parts-tree)
       (rtm/update-children)))
