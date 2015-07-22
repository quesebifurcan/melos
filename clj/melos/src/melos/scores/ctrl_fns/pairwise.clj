(ns melos.scores.ctrl-fns.pairwise
  (:require [melos.tools.dissonance-calculator :refer [scaled-dissonance-value]]
            [melos.tools.modify-durations :as mod-dur]))

(defn dissonant-melody-movement-mod
  [pair]
  (let [melodic-notes (mapcat
                       (fn [x] (filter #(= (:count %) 0) x))
                       pair)
        melodic-notes (filter :dissonance-contributor?
                              melodic-notes)
        melodic-notes (partition-by :part melodic-notes)
        melodic-notes (map
                       (fn [x]
                         (first (filter number? (map :pitch x))))
                           melodic-notes)]
    (if (> (scaled-dissonance-value melodic-notes)
           (scaled-dissonance-value [0 2 4]))
      (let [[a b]
            (map (fn [events dur]
                   (map (fn [event]
                          (assoc event
                                 :delta-dur dur
                                 :duration dur))
                        events))
                 pair
                 [4/4 3/4])]
        [a b]))))

(defn sustain-dissonant-melody
  [events]
  (mod-dur/modify-durations events [dissonant-melody-movement-mod]))

