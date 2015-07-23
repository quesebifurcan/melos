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

(defn dissonance->durations
  [vertical-moment]
  (let [pitches (map :pitch vertical-moment)
        diss-value (scaled-dissonance-value pitches)
        diss->dur (cond
                    (< diss-value
                       (scaled-dissonance-value [0 7]))
                    6/4
                    (< diss-value
                       (scaled-dissonance-value [0 4]))
                    4/4
                    (< diss-value
                       (scaled-dissonance-value [0 2 4]))
                    3/4
                    (< diss-value
                       (scaled-dissonance-value [0 1 2 4]))
                    2/4
                    :else
                    4/4)]
    diss->dur))

(defn dissonance->durations-mapping
  [events]
  (map (fn [vertical-moment]
         (let [dur (dissonance->durations vertical-moment)]
           (map #(assoc % :duration dur) vertical-moment)))
       events))
