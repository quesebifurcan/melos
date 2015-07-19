(ns melos.scores.ctrl-fns.stepwise
  (:require [melos.tools.dissonance-calculator :refer [scaled-dissonance-value]]))

(defn sustain-dissonant-vertical-moment
  [vertical-moment]
  (let [pitches (map :pitch vertical-moment)]
    (if (> (scaled-dissonance-value pitches)
           (scaled-dissonance-value [0 1]))
      (map #(assoc % :duration 8/4) vertical-moment)
      vertical-moment)))

(defn sustain-dissonant-vertical-moments
  [events]
  (map sustain-dissonant-vertical-moment events))
