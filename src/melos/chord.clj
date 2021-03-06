(ns melos.chord
  (:require [clojure.algo.generic.functor :as functor]
            [clojure.math
             [combinatorics :as combinatorics]
             [numeric-tower :as math]]
            [melos
             [note :as note]
             [schemas :as ms]
             [utils :refer [triangular-n]]]
            [schema.core :as s])
  (:import [melos.schemas Chord Note]))

(def pitch-type (atom :pitchclass))

(defn select-chord-key [k chord] (map k (:events chord)))

(defn set-chord-key
  [k v chord]
  (update chord :events
          (fn [events]
            (map (fn [note] (assoc note k v)) events))))

(defn chord-default
  []
  {:duration 1
   :tempo 60
   :phrase-end? false
   :is-rest? false
   :check-dissonance true
   :type :Chord
   :events []})

(defn make-chord-sel
  [m]
  (cond (contains? m :pitches)
        :from-pitch-list
        :default
        :from-event-list))

(defmulti make-chord make-chord-sel)

(s/defmethod make-chord :default
  [m]
  (ms/map->Chord (merge (chord-default) m)))

(s/defmethod make-chord :from-pitch-list
  :- Chord
  [m :- s/Any]
  (let [pitches      (:pitches m)
        m            (merge (chord-default) m)
        note-params  (select-keys m (keys (note/note-default)))
        chord-params (select-keys m (keys (chord-default)))
        group        (or (:group m) (gensym "G__"))
        events       (map (fn [pitch]
                            (note/make-note (merge note-params {:type :Note
                                                                :group group
                                                                :pitch pitch})))
                          pitches)]
    (ms/map->Chord (merge chord-params {:events events}))))

(defn contains-part?
  [part-names]
  (fn [event]
    (contains? part-names (:part event))))

(defn remove-parts*
  [part-names events]
  (remove (contains-part? part-names) events))

;; (s/defn remove-parts
;;   [part-names :- [s/Keyword]
;;    chord :- Chord]
;;   (update-in chord [:events] (fn [x] (remove-parts* (set part-names) x))))

(defn remove-parts
  [part-names chord]
  (let [grouped-events (partition-by :group (sort-by :group (:events chord)))
        events
        (filter (fn [group]
                  (empty? (clojure.set/intersection (set (map :part group))
                                                    (set part-names))))
                grouped-events)]
    (assoc chord :events (apply concat events))))



(defn get-melodic-events
  [chord]
  (->> chord
       :events
       (filter (comp zero? :count))
       set))

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

(defn normalize-ratio [ratio]
  (cond (< ratio 1)
        (normalize-ratio (* ratio 2))
        (> ratio 2)
        (normalize-ratio (/ ratio 2))
        :else
        ratio))

(defn rationalize' [n] (clojure.lang.Numbers/toRatio (rationalize n)))

(defn ratio-dissonance-score
  [ratio]
  (let [ratio' (normalize-ratio ratio)]
    (+ (numerator (rationalize' ratio'))
       (denominator (rationalize' ratio')))))

(defn dissonance-value
  [mapping intervals]
  (cond (= @pitch-type :pitchclass)
        (->> intervals
             inversion-equivalent-pitchclasses
             (map mapping)
             (apply +))
        (= @pitch-type :ratio)
        (->> intervals
             (map ratio-dissonance-score)
             (apply +))))

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
  (let [groups (partition-by :group (sort-by :group events))
        candidates (combinatorics/combinations groups (dec (count groups)))]
    (map #(apply concat %) candidates)))

(s/defn valid-events?
  :- s/Bool
  [mapping :- ms/DissonanceMapping
   limit   :- [s/Int]
   xs      :- [Note]]
  (boolean (some #(zero? (:count %)) xs)))

(s/defn reduce-dissonance'
  :- Chord
  [mapping :- ms/DissonanceMapping
   limit   :- [s/Int]
   chord   :- Chord]
  (let [events (->> chord
                    :events
                    get-candidates
                    (filter (partial valid-events? mapping limit))
                    (sort-by (fn [x] (apply + (map :count x)))))]
    (assoc chord :events (first events))))

;; (defn get-dissonance-contributor
;;   [chord]
;;   (update chord :events (fn [events]
;;                           (filter :dissonance-contributor? events))))

(s/defn reduce-dissonance
  :- Chord
  [mapping :- ms/DissonanceMapping
   limit   :- [s/Int]
   chord   :- Chord]
  (if (or (every? zero? (select-chord-key :count chord))
          (consonant? mapping limit (select-chord-key :pitch chord)))
    chord
    (reduce-dissonance mapping limit (reduce-dissonance' mapping limit chord))))
