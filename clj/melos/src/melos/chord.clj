(ns melos.chord
  (:require [clojure.algo.generic.functor :as functor]
            [clojure.math
             [combinatorics :as combinatorics]
             [numeric-tower :as math]]
            [melos
             [schemas :as ms]
             [utils :refer [triangular-n]]]
            [schema.core :as s]))

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

(s/defn uniquify-pitches-in-chord
  :- #{ms/Pitch}
  [chord :- [ms/Pitch]]
  (into #{} (map #(rem % 12) chord)))

(s/defn inversion-equivalent-pitchclass
  :- ms/Pitch
  [pc :- ms/Pitch]
  (let [pc (rem pc 12)]
    (if (> pc 6) (- 12 pc) pc)))

(s/defn inversion-equivalent-pitchclasses
  :- [ms/Pitch]
  [pitches :- [ms/Pitch]]
  (map inversion-equivalent-pitchclass pitches))

(s/defn all-intervals
  :- [ms/Pitch]
  [pitches :- #{ms/Pitch}]
  (map #(math/abs (apply - %))
       (combinatorics/combinations pitches 2)))

(s/defn calc-dissonance-divisor
  :- s/Int
  [pitches :- [ms/Pitch]]
  (-> pitches
      (uniquify-pitches-in-chord)
      (count)
      (triangular-n)))

(s/defn dissonance-value-partial
  [mapping :- ms/DissonanceMapping]
  (s/fn :- s/Num
    [pitches :- [ms/Pitch]]
    (->> pitches
         (uniquify-pitches-in-chord)
         (all-intervals)
         (inversion-equivalent-pitchclasses)
         (map mapping)
         (apply +))))

(def dissonance-value
  (atom (dissonance-value-partial dissonance-map-default)))

(s/defn scaled-dissonance-value
  :- s/Num
  [pitches :- [ms/Pitch]]
  (if (empty? pitches)
    0
    (let [divisor (calc-dissonance-divisor pitches)]
      (/ (@dissonance-value pitches) divisor))))

