(ns melos.main
    (:require [schema.core :as s]
              [melos.tools.utils :refer [export-to-json]]
              [melos.scores.graphs.score-graph :as score-graph]
              [melos.scores.compose-score :refer [compose-score]]
              [melos.scores.segments.segment :as segment]))

(s/set-fn-validation! true)

(time
 (export-to-json "/Users/fred/Desktop/score.json"
                 (compose-score (segment/initial-score-segment)
                                (segment/changes)
                                score-graph/compose-segment)))
