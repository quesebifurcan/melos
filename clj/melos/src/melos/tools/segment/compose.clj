(ns melos.tools.segment.compose
  (:require [melos.tools.segment.graphs :as graphs]))

(defn compose-event-seq
  [segment-config]
  (->> segment-config
       (graphs/lazy-segment-graph)
       (:result)))

(defn apply-rhythm
  [rhythm-config chord-seq]
  (-> rhythm-config
      (assoc :event-seq chord-seq)
      (graphs/lazy-rtm-graph)
      (:result)))
