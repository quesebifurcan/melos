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
  (:import [melos.lib.schemas Note Chord]))

(defn select-chord-key [k chord] (map k (:events chord)))

(def chord-default
  {:duration 1
   :tempo 60
   :phrase-end false
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

(defn get-melodic-events
  [chord]
  (->> chord
       :events
       (filter (comp zero? :count))
       set))

;; OLD

(declare get-melodic-event)

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

(defn pitch->pitchclass [p] (rem (+ 60 p) 12))
(defn pitches->pitchclasses [chord] (set (map pitch->pitchclass chord)))

(defn inversion-equivalent-pitchclass
  [pc]
  (let [pc (rem pc 12)]
    (if (> pc 6) (- 12 pc) pc)))

(defn inversion-equivalent-pitchclasses
  [pitches]
  (map inversion-equivalent-pitchclass pitches))

(defn all-intervals
  [pitches]
  (-> pitches
      pitches->pitchclasses
      (combinatorics/combinations 2)
      set))

(defn dissonance-value
  [mapping intervals]
  (->> intervals
       inversion-equivalent-pitchclasses
       (map mapping)
       (apply +)))

(defn interval->num [[a b]] (math/abs (- a b)))

(defn scaled-dissonance-value
  [mapping pitches]
  (let [intervals (map interval->num (all-intervals pitches))]
    (if (< (count intervals) 1)
      0
      (/ (dissonance-value mapping intervals)
         (count intervals)))))

(defn consonant?
  [mapping limit pitches]
  (let [limit (scaled-dissonance-value mapping limit)]
    (<= (scaled-dissonance-value mapping pitches) limit)))

;;-----------------------------------------------------------------------------
;; Reduce dissonance gradually
;;-----------------------------------------------------------------------------

(defn sum-counts [xs] (apply + (map :count xs)))

(defn get-candidates
  [events]
  (combinatorics/combinations events (dec (count events))))

(s/defn valid-events?
  :- s/Bool
  [mapping :- ms/DissonanceMapping
   limit   :- [s/Int]
   xs      :- [Note]]
  (boolean (and (some #(zero? (:count %)) xs)
                (consonant? mapping limit (map :pitch xs)))))

(s/defn reduce-dissonance'
  :- Chord
  [mapping :- ms/DissonanceMapping
   limit   :- [s/Int]
   chord   :- Chord]
  (let [events (->> chord
                    :events
                    get-candidates
                    (filter (partial valid-events? mapping limit)))]
    (assoc chord :events (first events))))

;; 1. calculate consonant candidates
;; 2. pick the one candidate with the least sum of :count
(s/defn reduce-dissonance
  :- Chord
  [mapping :- ms/DissonanceMapping
   limit   :- [s/Int]
   chord   :- Chord]
  (if (consonant? mapping limit (select-chord-key :pitch chord))
    chord
    (reduce-dissonance mapping limit (reduce-dissonance' mapping limit chord))))
