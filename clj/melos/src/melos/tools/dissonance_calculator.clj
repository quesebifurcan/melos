(ns melos.tools.dissonance-calculator
  (:require [clojure.math.combinatorics :as combinatorics]
            [clojure.algo.generic.functor :as functor]
            [clojure.math.numeric-tower :as math]
            [melos.tools.utils :refer [triangular-n]]))

(def dissonance-map
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
         1 6,
         2 4,
         3 3,
         4 2,
         5 1,
         6 5}]
    (functor/fmap #(math/expt % 10/9)
                  interval->dissonance)))

(defn- uniquify-pitches-in-chord
  "Given a chord, return a set of all its pitchclasses."
  [chord]
  (into #{} (map #(rem % 12) chord)))

(defn- inversion-equivalent-pitchclass
  "Given a pitchclass *pc*, convert it to an inversion-equivalent
  pitchclass. See dissonance-map for further explanation."
  [pc]
  (let [pc (rem pc 12)]
    (if (> pc 6) (- 12 pc) pc)))

(defn- inversion-equivalent-pitchclasses [pitches]
  (map inversion-equivalent-pitchclass pitches))

(defn- all-intervals
  "Return a collection of all intervals which can be formed between
  *pitches*."
  [pitches]
  (map #(math/abs (apply - %))
       (combinatorics/combinations pitches 2)))

(defn- calc-dissonance-divisor
  "In order to be able to use the same dissonance-values for chords
  with different numbers of notes, we need to scale the
  dissonance-value of the chord based on the number of pitches it
  contains. Turns out that the triangular series is appropriate for
  that."
  [pitches]
  (-> pitches
      (uniquify-pitches-in-chord)
      (count)
      (triangular-n)))
      
(defn- dissonance-value-fn
  "Given a mapping between intervals and dissonance-values, the
  collection *pitches* can be assigned a dissonance-value."
  [mapping]
  (fn [pitches]
    (->> pitches
         (uniquify-pitches-in-chord)
         (all-intervals)
         (inversion-equivalent-pitchclasses)
         (map mapping)
         (apply +))))

(def dissonance-value (dissonance-value-fn dissonance-map))

(defn scaled-dissonance-value
  "Scale dissonance-value based on the number of pitches."
  [pitches]
  (let [divisor (calc-dissonance-divisor pitches)]
    (/ (dissonance-value pitches) divisor)))






