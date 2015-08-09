(ns melos.chord-seq
  (:require [clojure.math
             [combinatorics :as combinatorics]
             [numeric-tower :as math]]
            [melos
             [chord :as diss-calc]
             [schemas :as ms]
             [utils :refer [rotate]]]
            [schema.core :as s]))

;; Collect events in segment.

(defn get-melodic-segment
  [part-seq part->event]
  (map part->event part-seq))

(defn get-and-rotate
  [melody-sources accessor]
  (let [event (first (get-in @melody-sources [accessor]))]
    (do (swap! melody-sources
               update-in
               [accessor]
               (partial drop 1))
        event)))

(defn collect-events-in-segment
  [melodic-indices events-seqs]
  (mapcat (fn [x] (get-and-rotate events-seqs x))
          melodic-indices))

;; Merge horizontally.

(s/defn can-merge?
  :- s/Bool
  [curr :- ms/Chord
   next :- ms/Chord]
  (let [old-curr (filter #(> (:count %) 0) next)
        news (filter #(= (:count %) 0) next)]
    (and (= (count curr) (count old-curr))
         (every? #(:merge-left? %) news)
         (every? #(:merge-right? %) old-curr))))

(s/defn merge-elts
  :- ms/Chord
  [a :- ms/Chord
   b :- ms/Chord]
  (let [melodic-notes (filter #(= (:count %) 0) b)]
    (concat a melodic-notes)))

(s/defn maybe-merge
  :- [ms/Chord]
  ([events :- [ms/Chord]]
   (if (seq events)
     (maybe-merge (first events)
                  (rest events))))
  ([head :- ms/Chord
    events :- [ms/Chord]]
   (cond (empty? events)
         (list head)
         (can-merge? head (first events))
         (maybe-merge (merge-elts head
                                  (first events))
                      (rest events))
         :else
         (cons head (maybe-merge events)))))

;; Extend events.

(s/defn filter-dissonance-contributors
  :- [s/Int]
  [vertical-moment :- ms/Chord]
  (->> vertical-moment
       (filter #(:dissonance-contributor? %))
       (map :pitch)
       (filter number?)))

(s/defn dissonance-value
  :- s/Num
  [vertical-moment :- ms/Chord]
  (diss-calc/scaled-dissonance-value
   (filter-dissonance-contributors vertical-moment)))

(s/defn consonant?
  :- s/Bool
  [vertical-moment :- ms/Chord
   limit :- [s/Int]]
  (let [limit (diss-calc/scaled-dissonance-value limit)]
    (<= (dissonance-value vertical-moment) limit)))

(s/defn zero-count?
  :- s/Bool
  [note :- ms/Note]
  (or (= 0 (:count note))
      (not (:dissonance-contributor? note))))

(s/defn contains-zero-count
  :- s/Bool
  [vertical-moment :- ms/Chord]
  ((complement nil?)
   (some #(= % 0) (map :count vertical-moment))))

(s/defn get-candidates
  [events]
  (combinatorics/combinations events
                              (- (count events) 1)))

(s/defn find-best-candidate
  :- ms/Chord
  [f :- s/Any
   events :- ms/Chord
   limit :- s/Num]
  (let [candidates (get-candidates events)]
    (->> candidates
         (filter contains-zero-count)
         (sort-by dissonance-value)
         (first)
         (f limit))))

(s/defn filter-by-count
  :- ms/Chord
  [limit :- s/Num
   events :- ms/Chord]
  (if (<= (count events) limit)
    events
    (find-best-candidate filter-by-count events limit)))

(s/defn filter-by-count-aggressive
  :- ms/Chord
  [limit :- s/Num
   events :- ms/Chord]
  (if (<= (count events) limit)
    events
    (filter zero-count? events)))

(s/defn filter-by-time-in-vertical-moment
  :- ms/Chord
  [limit :- s/Num
   events :- ms/Chord]
  (filter #(< (:count %) limit) events))

(s/defn all-parts-present?
  :- s/Bool
  [part-names :- [ms/PartName]
   events :- ms/Chord]
  (= (set part-names)
     (set (map :part events))))

(s/defn best-part-match
  :- [ms/Chord]
  [events :- ms/Chord
   candidates :- [ms/Chord]]
  (let [result (filter (partial all-parts-present?
                                (map :part events))
                       candidates)]
    (if (empty? result)
      candidates
      result)))

(s/defn total-count
  :- s/Int
  [events :- ms/Chord]
  (apply + (map :count events)))

(s/defn group-events
  :- [ms/Chord]
  [events :- ms/Chord]
  (->> events
       (sort-by :group)
       (partition-by :group)))

(s/defn filter-by-dissonance-value
  :- ms/Chord
  [limit events]
  (let [grouped-events (group-events events)]
    (if (or (< (count grouped-events) 2)
            (consonant? events limit))
      events
      (let [candidates (->> (get-candidates grouped-events)
                            (map flatten)
                            (filter contains-zero-count)
                            (best-part-match events)
                            (filter #(consonant? % limit))
                            (sort-by total-count)
                            ;; (sort-by (fn [x]
                            ;;            (let [pitches (map :pitch x)]
                            ;;              (diss-calc/scaled-dissonance-value pitches))))
                            ;; (sort-by #(count %))
                            ;; (reverse)
                            (first))]
        (if (empty? candidates)
          ;; If no candidates are valid, return a vector with the most
          ;; recently added events.
          (filter zero-count? events)
          (recur limit candidates))))))

(s/defn forward-time
  :- ms/Chord
  [events :- ms/Chord]
  (->> events
       (filter :allow-extension?)
       (map #(update-in % [:count] inc))))

(s/defn join-events
  :- ms/Chord
  [new-event :- ms/Chord
   events :- ms/Chord]
  ;; TODO: filter distinct events.
  (concat events new-event))

(s/defn coll-part-counts-map
  [vertical-moment :- ms/Chord]
  (let [partitioned-events (->> vertical-moment
                                (sort-by :part)
                                (partition-by :part))]
    (zipmap (map (comp :part first) partitioned-events)
            (map (comp :max-part-count first) partitioned-events))))

(s/defn filter-parts-by-count
  :- ms/Chord
  [events :- ms/Chord]
  (let [part-counts (coll-part-counts-map events)]
    (mapcat (fn [[part-name limit]]
              (let [xs (filter #(= (:part %) part-name) events)]
                (if (<= (count xs) limit)
                  xs
                  (filter #(= (:count %) 0) xs))))
            part-counts)))

(defn filter-part-idiomatic
  [vertical-moment]
  (let [melodic-event (first (filter #(= 0 (:count %)) vertical-moment))]
    (if (empty? melodic-event)
      vertical-moment
      (filter #(< (- (max (:pitch melodic-event) (:pitch %))
                     (min (:pitch melodic-event) (:pitch %)))
                  12)
              vertical-moment))))

(defn filter-idiomatic
  [vertical-moment]
  (let [groups (->> (sort-by :part vertical-moment)
                    (partition-by :part))]
    (mapcat filter-part-idiomatic groups)))

(defn event-count-ok?
  [{:keys [count max-count]}]
  (<= count max-count))

(defn handle-dissonance
  "Return a function which can be used to control dissonance values in
  one segment of the piece."
  [{:keys [max-count diss-value max-lingering] :as m}]
  (s/validate ms/DissonanceFnParams m)
  (fn [events event]
    (->> events
         (forward-time)
         (join-events event)
         ;; (filter-by-count-aggressive max-count)
         (filter event-count-ok?)
         (filter-idiomatic)
         (filter-parts-by-count)
         (filter-by-time-in-vertical-moment max-lingering)
         (filter-by-dissonance-value diss-value))))

(defn extend-events
  [diss-fn-params events]
  (-> (handle-dissonance diss-fn-params)
      (reductions [] events)
      (rest)))

;; Modify durations.

(s/defn pairwise-mod
  :- [ms/Chord]
  [chords :- [ms/Chord]
   tests :- [s/Any]
   coll :- [ms/Chord]]
  (let [pair (take 2 chords)]
    (cond (empty? pair)
          coll
          :else
          (let [result (drop-while
                        nil?
                        (map #(% pair) tests))]
            (if (empty? result)
              (pairwise-mod (rest chords)
                            tests
                            (concat coll [(first chords)]))
              (pairwise-mod (drop 2 chords)
                            tests
                            (concat coll (into [] (first result)))))))))

(s/defn modify-durations
  :- [ms/Chord]
  [chords :- [ms/Chord]
   mod-fns :- [s/Any]]
  (pairwise-mod chords mod-fns []))
