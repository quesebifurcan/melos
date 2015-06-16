(ns melos.scores.tools
  (:require [schema.core :as s]
            [melos.tools.rtm :as rtm]
            [melos.tools.utils :refer [merge-in]]
            [melos.tools.schemata :as schemata]))

(s/defn unfold-segments
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
  [initial-score-segment changes comp-fn]
  (->> (unfold-segments initial-score-segment changes)
       (map comp-fn)
       (map :result)))

