(ns melos.scores.utils.unfold-segments
  (:require [schema.core :as s]
            [melos.tools.utils :refer [merge-in]]
            [melos.tools.schemata :as schemata]))

(s/defn unfold-segments
  :- [schemata/ScoreSegment]
  [init :- schemata/ScoreSegment
   changes :- [schemata/PartialScoreSegment]]
  (->> (reductions merge-in init changes)
       (rest)))
