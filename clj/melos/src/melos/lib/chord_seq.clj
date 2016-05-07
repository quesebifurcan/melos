(ns melos.lib.chord-seq
  (:require [clojure.math
             [combinatorics :as combinatorics]
             [numeric-tower :as math]]
            [clojure.set :as set]
            [melos.lib
             [note :as note]
             [chord :as chord]
             [schemas :as ms]
             [utils :refer [rotate distinct-by]]]
            [schema.core :as s]))

(defn get-melodic-durations
  [chords]
  (map chord/get-melodic-duration chords))

(defn sum-melodic-durations
  [chords]
  (reduce + 0 (get-melodic-durations chords)))

;;-----------------------------------------------------------------------------
;; Collect events in segment.

(defn get-melodic-segment
  [part-seq part->event]
  (map part->event part-seq))

(defn get-and-rotate
  [melody-sources accessor]
  (let [event (first (get-in @melody-sources [accessor]))]
    (swap! melody-sources update-in [accessor] (partial drop 1))
    event))

(defn collect-events-in-segment
  [melodic-indices events-seqs]
  (map (fn [x] (get-and-rotate events-seqs x))
       melodic-indices))

;; (chord-seq/collect-events-in-segment
;;  [:a :a]
;;  (atom
;;   {:a [[1 2 3] [4 5 6]]}))
;; => (1 2 3 4 5 6)

;;-----------------------------------------------------------------------------
;; Extend events.

(defn filter-dissonance-contributors
  [chord]
  (->> chord
       (chord/dissonance-contributors)
       (map :pitch)
       (filter number?)))

(defn dissonance-value
  [chord]
  (chord/scaled-dissonance-value
   (filter-dissonance-contributors chord)))

(defn consonant?
  [chord limit]
  (let [limit (chord/scaled-dissonance-value limit)]
    (<= (dissonance-value chord) limit)))

(defn forward-time
  [events]
  (map #(update-in % [:count] inc) events))

(defn filter-distinct
  [chord]
  (->> chord
       (sort-by (juxt :part :pitch))
       (partition-by (juxt :part :pitch))
       (map (fn [x] ((comp first (partial sort-by :count)) x)))))

(defn join-events
  [new-event events]
  (let [duration (:duration (first new-event))
        curr-parts (set (map :part new-event))
        events (filter (fn [event]
                         (not (contains? curr-parts (:part event))))
                       events)]
    (->> (concat (map #(assoc % :duration duration)
                      events)
                 new-event))))

(declare extend-events)
(declare handle-dissonance)

(defn extend-phrases
  ([diss-fn-params]
   (extend-phrases diss-fn-params [] []))
  ([diss-fn-params coll phrases]
  (if (empty? phrases)
    coll
    (let [first-phrase (first phrases)
          last_ (map #(update % :count inc) (last coll))
          ;; last_ (map (fn [x] x) (last coll))
          next_ (map #(join-events % last_)
                     first-phrase)]
      (let [non-rests (remove :is-rest? (last next_))]
      (if (or (empty? non-rests)
              (consonant? non-rests
                          (:diss-params diss-fn-params)))
        (extend-phrases diss-fn-params
                        (concat coll
                                  next_)
                        (rest phrases))
        (extend-phrases diss-fn-params
                        (if (empty? coll)
                          next_
                          (concat (butlast coll)
                                  [(map (fn [x] (assoc x :phrase-end true))
                                        (last coll))]
                                  first-phrase )
                          )
                       (rest phrases))))))))

;; (defn extend-chords
;;   ([pred merge-fn xs]
;;    (extend-chords pred
;;                   merge-fn
;;                   (first xs)
;;                   (rest xs)))
;;   ([pred merge-fn head tail]
;;    (let [nxt (first tail)]
;;      (cond (nil? head) []
;;            (nil? nxt) [head]
;;            ;; TODO: add clause for consing existing head
;;            :else
;;            (recur (concat head nxt)
;;                   (rest tail))))))

;;-----------------------------------------------------------------------------
;; Merge events horizontally.

(defn can-merge?
  [curr next]
  (let [curr (distinct-by #((juxt :pitch :part) %) curr)
        next (distinct-by #((juxt :pitch :part) %) next)
        old-curr (filter #(pos? (:count %)) next)
        news (filter #(zero? (:count %)) next)
        old-parts (set (map :part old-curr))
        new-parts (set (map :part news))
        curr-blocking (->> curr (filter #(contains? new-parts (:part %)))
                           (filter #(zero? (:count %))))]
    (let [result
          (and (empty? curr-blocking)
               ;; Make sure that two sequential events in one part are not merged.
               (empty? (clojure.set/intersection old-parts new-parts))
               (every? :merge-left? news)
               (every? :merge-right? old-curr))]
          result)))

(defn merge-elts
  [a b]
  (let [melodic-notes (filter #(zero? (:count %)) b)
        new-parts (set (map :part melodic-notes))
        new-a (filter #(not (contains? new-parts (:part %))) a)
        result (concat new-a melodic-notes)]
   result))

(defn merge-horizontally
  ([events]
   (if (seq events)
     (merge-horizontally (first events)
                         (rest events))))
  ([head events]
   (cond (empty? events)
         (list head)
         ;; TODO: add harmonic filter to merge.
         (and (can-merge? head (first events))
              (consonant? (merge-elts head
                                      (first events))
                          [0 2 4 5]))
         (merge-horizontally (merge-elts head
                                         (first events))
                             (rest events))
         :else
         (cons head (merge-horizontally events)))))
