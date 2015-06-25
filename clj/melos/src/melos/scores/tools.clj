(ns melos.scores.tools
  (:require [schema.core :as s]
            [melos.tools.rtm :as rtm]
            [melos.tools.make-note :refer [make-note]]
            [melos.tools.utils :refer [merge-in rotate]]
            [melos.tools.schemata :as ms]
            [schema.core :as s]))

(defn unfold-events
  [m]
  (let [f (:fn m)
        partition-fn (:partition m)
        m (dissoc m :fn :partition)]
  (->> (map (fn [x] x)
            (vals m))
       (map cycle)
       (apply map vector)
       (map (fn [x] (zipmap (keys m) x)))
       (map f)
       (partition-fn)
       (map #(s/validate [ms/VerticalMoment] %)))))

(s/defn unfold-segments
  :- [ms/ScoreSegment]
  [init :- ms/ScoreSegment
   changes :- [ms/PartialScoreSegment]]
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

