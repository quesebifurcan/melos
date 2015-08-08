(ns melos.tools.chord-seq.selector-sequence
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
  [melodic-indices events-seqs]
  (mapcat (fn [x] (get-and-rotate events-seqs x))
          melodic-indices))

