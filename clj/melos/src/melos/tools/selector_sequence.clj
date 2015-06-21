(ns melos.tools.selector-sequence
  (:require [melos.tools.utils :refer [rotate]]))

(defn get-melodic-segment
  [part-seq part->event]
  (map part->event part-seq))

(defn get-and-rotate
  [melody-sources accessor]
  (let [event (first (get-in @melody-sources [accessor]))]
    (do (swap! melody-sources
               update-in
               [accessor]
               (partial drop 1))
        event)))

(defn collect-events-in-segment
  "Collect melodic events according to score. The rather convoluted
  logic is explained by the fact that:

  - the score is divided into segments and
  - the state of the melodic sources needs to be passed from one
  segment to the next.

  In other words, the actual value of a reference to a melodic event
  changes depending on the state. For example, repeated use of [:lower
  :lindenmayer] as accessor will yield a sequence of different melodic
  events."
  [melodic-indices events-seqs]
  (mapcat (fn [x] (get-and-rotate events-seqs x))
          melodic-indices))
