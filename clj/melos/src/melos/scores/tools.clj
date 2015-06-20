(ns melos.scores.tools
  (:require [schema.core :as s]
            [melos.tools.rtm :as rtm]
            [melos.tools.make-note :refer [make-note]]
            [melos.tools.utils :refer [merge-in
                                       rotate]]
            [melos.tools.schemata :as schemata]))


(defn make-chord-from-pitch-vector-params
  [{:keys [pitch] :as m}]
  (let [group (gensym "G__")]
  (map (fn [p]
         (make-note (merge {:pitch p :group group}
                           (dissoc m :pitch))))
       pitch)))

(defn parse-params
  [x]
  (if (list? x)
    (repeat (last x) (first x))
    [x]))

(defn unfold-events
  [m]
  (let [f (:fn m)
        partition-fn (:partition m)
        m (dissoc m :fn :partition)]
  (->> (map (fn [x] (mapcat parse-params x))
            (vals m))
       (map cycle)
       (apply map vector)
       (map (fn [x] (zipmap (keys m) x)))
       (map f)
       (partition-fn))))

(defn cyclic-partition
  [xs splits]
  (cons (take (first splits) xs)
        (lazy-seq (cyclic-partition (drop (first splits) xs)
                                    (rotate splits)))))

(defn cyclic-repeats
  [xs repeats]
  (if (seq xs)
    (concat (repeat (first repeats) (first xs))
            (lazy-seq (cyclic-repeats (rotate xs)
                                      (rotate repeats))))))

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

