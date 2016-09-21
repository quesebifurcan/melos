(ns melos.chord-seq
  (:require [clojure.algo.generic.functor :as functor]
            [clojure.math
             [combinatorics :as combinatorics]
             [numeric-tower :as math]]
            [clojure.set :as set]
            [melos
             [chord :as chord]
             [note :as note]
             [schemas :as ms]
             [utils :refer [distinct-by partition-by-inclusive rotate]]]
            [schema.core :as s]
            [clojure.spec :as spec])
  (:import melos.schemas.Chord))

(defn update-events
  [chord k f]
  (update chord :events (fn [events]
                          (map (fn [event] (update event k f))
                               events))))

(s/defn merge-chords
  :- Chord
  [a :- Chord
   b :- Chord]
  (let [new-part-names (chord/select-chord-key :part b)]
    (update b :events (fn [events] (->> (update-events a :count inc)
                                        (chord/remove-parts new-part-names)
                                        :events
                                        (concat events))))))

;;-----------------------------------------------------------------------------
;; Collect events in segment.

(defn get-and-rotate
  [melody-sources accessor]
  (if-let [event (first (get-in @melody-sources [accessor]))]
    (do (swap! melody-sources update-in [accessor] rest)
        event)
    (throw (Exception. (format "No key found for accessor %s" accessor)))))

(defn cycle-event-seqs'
  [accessors event-seqs]
  (mapcat (fn [accessor] (get-and-rotate event-seqs accessor))
          accessors))

(defn atom? [x] (instance? clojure.lang.Atom x))

(s/defn cycle-event-seqs
  :- ms/Phrase
  [accessors  :- [s/Keyword]
   event-seqs :- (s/pred atom?)]
  (cycle-event-seqs' accessors event-seqs))

(defn maybe-extend
  [pred merge-fn]
  (fn [a b]
    (if (pred a b) (merge-fn a b) b)))

;; (defn partition-phrases
;;   [xs]
;;   (filter identity
;;           (partition-by-inclusive (complement :phrase-end) xs)))

(defn merge-adjacent?
  ([a b]
   (merge-adjacent? (fn [_ _] true) a b))
  ([pred a b]
   (let [a' (chord/get-melodic-events a)
         b' (chord/get-melodic-events b)]
     (and (pred a b)
          (every? :merge-right? a')
          (every? :merge-left? b')
          (empty? (clojure.set/intersection
                   (set (map :part a'))
                   (set (map :part b'))))))))

(defn merge-horizontally
  ([consonance-pred events]
   (if (seq events)
     (merge-horizontally consonance-pred (first events) (rest events))))
  ([consonance-pred head events]
   (cond (empty? events)
         (list head)
         (merge-adjacent? consonance-pred head (first events))
         (merge-horizontally consonance-pred
                             (merge-chords head (first events))
                             (rest events))
         :else
         (cons head (merge-horizontally consonance-pred events)))))

(defn same?
  [a b]
  (let [a-groups (set (chord/select-chord-key :group a))
        b-groups (set (chord/select-chord-key :group b))
        a-pitches (set (chord/select-chord-key :pitch a))
        b-pitches (set (chord/select-chord-key :pitch b))]
    (and (= a-groups b-groups)
         (= a-pitches b-pitches))))

(defn event-seq-merger
  [merge-pred merge-fn]
  (fn inner
    ([xs]
     (if (seq xs)
       (inner (first xs) (rest xs))))
    ([x xs]
     (cond (empty? xs)
           (list x)
           (merge-pred x (first xs))
           (inner (merge-fn x (first xs)) (rest xs))
           :else
           (cons x (inner xs))))))

(defn merge-durations
  [a b]
  (update a :duration (fn [x] (+ x (:duration b)))))

(def simplify-event-seq (event-seq-merger same? merge-durations))
