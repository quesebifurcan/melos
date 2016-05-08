(ns melos.lib.chord
  (:require [clojure.algo.generic.functor :as functor]
            [clojure.math
             [combinatorics :as combinatorics]
             [numeric-tower :as math]]
            [melos.lib
             [note :as note]
             [schemas :as ms]
             [utils :refer [triangular-n]]]
            [schema.core :as s])
  (:import [melos.lib.schemas Chord]))

(defn select-chord-key [k chord] (map k (:events chord)))

(def chord-default
  {:duration 1
   :tempo 60
   :events []})

;; TODO: use select-keys complimentary function:
;; (def test (partial apply dissoc {:b 8 :a 2 :c 234}))

(defn make-chord-sel
  [m]
  (cond (contains? m :pitches)
        :from-pitch-list
        :default
        :from-event-list))

(defmulti make-chord make-chord-sel)

(s/defmethod make-chord :default
  [m]
  (ms/map->Chord (merge chord-default m)))

(s/defmethod make-chord :from-pitch-list
  :- Chord
  [m :- s/Any]
  (let [pitches      (:pitches m)
        m            (merge chord-default m)
        note-params  (select-keys m (keys (note/note-default)))
        chord-params (select-keys m (keys chord-default))
        group        (gensym "G__")
        events       (map (fn [pitch]
                            (note/make-note (merge note-params {:group group :pitch pitch})))
                          pitches)]
    (ms/map->Chord (merge chord-params {:events events}))))

(defn contains-part?
  [part-names]
  (fn [event]
    (contains? part-names (:part event))))

(defn remove-parts*
  [part-names events]
  (remove (contains-part? part-names) events))

(s/defn remove-parts
  [part-names :- [s/Keyword]
   chord :- Chord]
  (update-in chord [:events] (fn [x] (remove-parts* (set part-names) x))))

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