(ns melos.main
    (:require [schema.core :as s]
              [melos.tools.utils :refer [export-to-json]]
              [melos.scores.graphs.score-graph :as score-graph]
              [melos.scores.compose-score :refer [compose-score]]
              [melos.scores.segments.segment :as segment]))

(s/set-fn-validation! true)

(defn main
  [output-path]
  (time
   (export-to-json output-path
                   (compose-score (segment/initial-score-segment)
                                  (segment/changes)
                                  score-graph/compose-segment))))
