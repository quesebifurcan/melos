(ns melos.tools.calc-time-signatures
  (:require [clojure.math.combinatorics :as combinatorics]
            [melos.tools.utils :refer [sum]]))

(defn- forward-time
  "Increment :count and :total-dur of all elements in state."
  [state event]
  (let [increment-dur (fn [x] (+ x (:delta-dur event)))]
    (-> state
        (update-in [:curr-dur] increment-dur)
        (update-in [:total-dur] increment-dur))))

(defn- get-time-signature-candidates
  "Given a maximum lookahead (*curr-depth*), retrieve all possible
  sequences of *time-signatures*."
  [{:keys [time-signatures curr-depth]}]
  (combinatorics/selections time-signatures curr-depth))

(defn- get-matching-time-signatures
  "Return all *candidates* whose combined duration adds up to *dur*."
  [candidates dur]
  (filter #(= (sum %) dur) candidates))

(defn- overfilled?
  "Is the combined duration of every candidate in *candidates* longer
  than *dur*?"
  [candidates dur]
  (every? #(> (sum %) dur) candidates))

(defn- underfilled?
  "Is the combined duration every candidate in *candidates* shorter
  than *dur*? Also returns true if *candidates* is empty."
  [matching candidates dur]
  (or (every? #(< (sum %) dur) candidates)
      (empty? matching)))

(defn initial-state
  "The initial state used in find-time-signature below."
  [time-signatures]
  {:result []
   :curr-dur 0
   :total-dur 0
   :time-signatures time-signatures
   :curr-depth 1
   :overhang 0
   :curr-ts []})

(defn find-time-signature
  "Given a sequence of durations, find the best matching sequence of
  time signatures.

  Cases:

  - no match AND sum of curr-ts greater than sum of curr-dur: add
  another duration.
  - no match AND sum of curr-dur greater than sum of curr-ts: add
  another ts by increasing curr-depth.
  - match: collect
  - curr-depth > limit: throw exception.

  "
  [state event]
  (let [new-state (forward-time state event)
        curr-dur (:curr-dur new-state)
        curr-depth (:curr-depth new-state)
        candidates (get-time-signature-candidates new-state)
        matching (get-matching-time-signatures candidates curr-dur)]

    (cond (> curr-depth 10)
          (throw (Exception.
                  (str "Given a selection depth of 5, "
                       "could not find a match for duration")))

          (seq matching)
          (let [result (first matching)]
            (-> new-state
                (update-in [:result] (fn [x] (conj x result)))
                (assoc :curr-dur 0
                       :curr-depth 1
                       :curr-ts [])))

          (overfilled? candidates curr-dur)
          (-> new-state
              (assoc :curr-depth 1))

          (underfilled? matching candidates curr-dur)
          (recur (-> new-state
                     (update-in [:curr-depth] (fn [x] (inc x))))
                 {:delta-dur 0}))))

(defn- best-time-signature-candidate
  [overhang time-signatures]
  (->> time-signatures
       (sort-by #(- % overhang))
       (filter #(>= % overhang))
       (first)))

(defn- handle-overhang
  "If :curr-dur of :state is non-zero, we are dealing with an
  overhang -- there is no combination of time signatures which exactly
  matches the duration. In that case, find the best
  approximation. TODO: add padding with rests."
  [{:keys [curr-dur time-signatures] :as state}]
  (if (= curr-dur 0)
    state
    (let [overhang curr-dur
          best-match (best-time-signature-candidate overhang
                                                    time-signatures)]
      (update-in state
                 [:result]
                 (fn [x] (conj x [best-match]))))))

(defn calculate-time-signatures
  "Given a collection of allowed time-signatures *ts-options*,
  calculate the optimal sequence of time-signatures based on the
  durations of *events*."
  [ts-options events]
  (let [init (initial-state ts-options)]
    (->> (reduce find-time-signature init events)
         (handle-overhang)
         (:result)
         (flatten)
         (filter identity))))
