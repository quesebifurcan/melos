(ns melos.lib.chord
  (:require [clojure.algo.generic.functor :as functor]
            [clojure.math
             [combinatorics :as combinatorics]
             [numeric-tower :as math]]
            [melos.lib
             [schemas :as ms]
             [utils :refer [triangular-n]]]
            [schema.core :as s]))

(defn get-melodic-event
  [chord]
  (->> chord
       (filter #(zero? (:count %)))
       (first)))

(defn get-melodic-duration
  [chord]
  ((comp :duration get-melodic-event) chord))

(defn pitchset
  [chord]
  (set (map :pitch chord)))

(defn dissonance-contributors
  [chord]
  (filter :dissonance-contributor? chord))

(def dissonance-map-default
  "Map complementary intervals to relative dissonance values.

  (\"Complementary intervals\": the minor second is considered equally
  dissonant to the major seventh; the major second is considered
  equally dissonant to the minor seventh; the minor third is
  considered equally dissonant to the major sixth etc.)

  All dissonance values are raised to the power of 10/9. This is an
  arbitrarily chosen \"magic number\" -- the exact value of the
  dissonance values is not important; they just need to be
  sufficiently different.
  "
  (let [interval->dissonance
        {0 0,
         1 10,
         2 4,
         3 3,
         4 2,
         5 1,
         6 5}]
    (functor/fmap #(math/expt % 10/9)
                  interval->dissonance)))

(defn uniquify-pitches-in-chord
  [chord]
  (set (map #(rem % 12) chord)))

(defn inversion-equivalent-pitchclass
  [pc]
  (let [pc (rem pc 12)]
    (if (> pc 6) (- 12 pc) pc)))

(defn inversion-equivalent-pitchclasses
  [pitches]
  (map inversion-equivalent-pitchclass pitches))

(defn all-intervals
  [pitches]
  (map #(math/abs (apply - %))
       (combinatorics/combinations pitches 2)))

(defn calc-dissonance-divisor
  [pitches]
  (-> pitches
      (uniquify-pitches-in-chord)
      (count)
      (triangular-n)))

(defn dissonance-value-partial
  [mapping]
  (fn [pitches]
    (->> pitches
         (uniquify-pitches-in-chord)
         (all-intervals)
         (inversion-equivalent-pitchclasses)
         (map mapping)
         (apply +))))

(def dissonance-value
  (atom (dissonance-value-partial dissonance-map-default)))

(defn scaled-dissonance-value
  [pitches]
  (if (empty? pitches)
    0
    (let [divisor (calc-dissonance-divisor pitches)]
      (/ (@dissonance-value pitches) divisor))))

