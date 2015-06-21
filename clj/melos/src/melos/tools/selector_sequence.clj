(ns melos.tools.selector-sequence
  (:require [melos.tools.utils :refer [rotate]]))

(defn get-melodic-segment
  [part-seq part->event]
  (map (fn [x] [(x part->event)])
       part-seq))

(require '[melos.tools.utils :refer [rotate]])

(defn get-and-rotate
  [state accessor]
  (let [
        v (first (get-in @state accessor))]
    (do (swap! state update-in accessor (fn [x] (drop 1 x)))
        v)))

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
