(ns melos.lib.chord-seq
(:require [clojure.math
[combinatorics :as combinatorics]
[numeric-tower :as math]]
[clojure.set :as set]
[melos.lib
[chord :as chord]
[schemas :as ms]
[utils :refer [rotate]]]
[schema.core :as s]))

(s/defn get-melodic-durations
  :- [s/Num]
  [chords :- [ms/Chord]]
  (map chord/get-melodic-duration chords))

(s/defn sum-melodic-durations
  :- s/Num
  [chords :- [ms/Chord]]
  (reduce + 0 (get-melodic-durations chords)))

;;-----------------------------------------------------------------------------
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
  (map (fn [x] (get-and-rotate events-seqs x))
       melodic-indices))

;; (chord-seq/collect-events-in-segment
;;  [:a :a]
;;  (atom
;;   {:a [[1 2 3] [4 5 6]]}))
;; => (1 2 3 4 5 6)

;;-----------------------------------------------------------------------------
;; Merge events horizontally.

(s/defn can-merge?
  :- s/Bool
  [curr :- ms/Chord
   next :- ms/Chord]
  (let [old-curr (filter #(> (:count %) 0) next)
        news (filter #(= (:count %) 0) next)
        old-parts (set (map :part old-curr))
        new-parts (set (map :part news))]
    (and (= (count curr) (count old-curr))
         ;; Make sure that two sequential events in one part are not merged.
         (empty? (clojure.set/intersection old-parts new-parts))
         (every? #(:merge-left? %) news)
         (every? #(:merge-right? %) old-curr))))

(s/defn merge-elts
  :- ms/Chord
  [a :- ms/Chord
   b :- ms/Chord]
  (let [melodic-notes (filter #(= (:count %) 0) b)]
    (concat a melodic-notes)))

(s/defn merge-horizontally
  :- [ms/Chord]
  ([events :- [ms/Chord]]
   (if (seq events)
     (merge-horizontally (first events)
                         (rest events))))
  ([head :- ms/Chord
    events :- [ms/Chord]]
   (cond (empty? events)
         (list head)
         (can-merge? head (first events))
         (merge-horizontally (merge-elts head
                                         (first events))
                             (rest events))
         :else
         (cons head (merge-horizontally events)))))

;;-----------------------------------------------------------------------------
;; Extend events.

(s/defn filter-dissonance-contributors
  :- [s/Int]
  [chord :- ms/Chord]
  (->> chord
       (chord/dissonance-contributors)
       (map :pitch)
       (filter number?)))

(s/defn dissonance-value
  :- s/Num
  [chord :- ms/Chord]
  (chord/scaled-dissonance-value
   (filter-dissonance-contributors chord)))

(s/defn consonant?
  :- s/Bool
  [chord :- ms/Chord
   limit :- [s/Int]]
  (let [limit (chord/scaled-dissonance-value limit)]
    (<= (dissonance-value chord) limit)))

(s/defn zero-count?
  :- s/Bool
  [note :- ms/Note]
  (or (= 0 (:count note))
      (not (:dissonance-contributor? note))))

(s/defn contains-zero-count
  :- s/Bool
  [chord :- ms/Chord]
  ((complement nil?)
   (some #(= % 0) (map :count chord))))

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

(s/defn filter-by-time-in-chord
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

(defn all-but-oldest
  [events]
  (->> events
       (sort-by :count)
       (butlast)))

;; PENALTIES

(defn part-match-penalty
  [all-part-names events]
  (let [part-names (set (map :part events))]
    (- (count all-part-names)
       (count part-names))))

;; (defn dissonance-penalty
;;   [events]
;;   (let [pitches (map :pitch events)]
;;     (chord/scaled-dissonance-value pitches)))

(defn age-penalty
  [events]
  (apply + (map :count events)))

;; (def diss-params
;;   {:check (fn [events]
;;             (<= (chord/scaled-dissonance-value (map :pitch events))
;;                 (chord/scaled-dissonance-value [0 1 3])))
;;    :sort_ [part-match-penalty age-penalty]})

(defn normalize
  [xs]
  (let [max_ (apply max xs)
        min_ (apply min xs)
        div (- max_ min_)]
    (if (or (= 0 div)
            (apply = xs))
      (map (fn [x] 0) xs)
      (map (fn [x] (/ (- x min_) (- max_ min_)))
           xs))))

(defn transpose [m]
  (apply mapv vector m))

;; TODO: normalize -- compare to theoretical max/min for dissonance.

(defn filter-by-dissonance-value
  [diss-params events]
  (if (or ((:check diss-params) events)
          (<= (count (group-events events)) 1))
    events
    (let [grouped-events (group-events events)
          candidates (->> (get-candidates grouped-events)
                          (map flatten)
                          (filter contains-zero-count))
          all-parts (map :part events)
          part-match-scores (map (partial part-match-penalty all-parts)
                                 candidates)
          part-match-norm (->> (normalize part-match-scores)
                               (map (fn [x] (* x 3))))
          age-penalty-scores (map age-penalty candidates)
          age-penalty-norm (normalize age-penalty-scores)
          dissonance-scores (map (fn [x] (chord/scaled-dissonance-value
                                          (map :pitch x)))
                                 candidates)
          dissonance-norm (normalize dissonance-scores)
          sorted (sort-by (fn [[x y z zz]]
                            (+ y z))
                          (transpose [candidates part-match-norm age-penalty-norm dissonance-norm]))
          best (first (first sorted))]
      (if ((:check diss-params) best)
        best
        (recur diss-params (->> (sort-by (fn [x] (chord/scaled-dissonance-value
                                                  (map :pitch x)))
                                         candidates)
                                (first)))))))

(s/defn forward-time
  :- ms/Chord
  [events :- ms/Chord]
  (->> events
       ;; (filter :allow-extension?)
       (map #(update-in % [:count] inc))))

(defn filter-distinct
  [chord]
  (->> chord
       (sort-by (juxt :part :pitch))
       (partition-by (juxt :part :pitch))
       (map (fn [x] ((comp first (partial sort-by :count)) x)))))

(s/defn join-events
  :- ms/Chord
  ;; [new-event :- ms/Chord
  ;;  events :- ms/Chord]
  [new-event events]
  (let [curr-parts (set (map :part new-event))
        events (filter (fn [event]
                         (not (contains? curr-parts (:part event))))
                       events)]
  (->> (concat events new-event)
       ;; (filter-distinct)
       )))

;; (s/defn coll-part-counts-map
;;   [chord :- ms/Chord]
;;   (let [partitioned-events (->> chord
;;                                 (sort-by :part)
;;                                 (partition-by :part))]
;;     (zipmap (map (comp :part first) partitioned-events)
;;             (map (comp :max-part-count first) partitioned-events))))

;; (defn filter-part-by-count
;;   [xs limit]
;;   (if (<= (count xs) limit)
;;     xs
;;     (recur (butlast (sort-by :count xs)) limit)))

;; (s/defn filter-parts-by-count
;;   :- ms/Chord
;;   [events :- ms/Chord]
;;   (let [part-counts (coll-part-counts-map events)]
;;     (mapcat (fn [[part-name limit]]
;;               (let [xs (filter #(= (:part %) part-name) events)]
;;                 ;; (if (<= (count xs) limit)
;;                 ;;   xs
;;                 ;;   (filter #(= (:count %) 0) xs))))
;;                 (filter-part-by-count xs limit)))
;;             part-counts)))

;; (defn filter-part-idiomatic
;;   [chord]
;;   (let [melodic-event (chord/get-melodic-event chord)]
;;     (if (empty? melodic-event)
;;       chord
;;       (filter #(< (- (max (:pitch melodic-event) (:pitch %))
;;                      (min (:pitch melodic-event) (:pitch %)))
;;                   12)
;;               chord))))

;; (defn filter-idiomatic
;;   [chord]
;;   (let [groups (->> (sort-by :part chord)
;;                     (partition-by :part))]
;;     (mapcat filter-part-idiomatic groups)))

(defn event-count-ok?
  [{:keys [count max-count]}]
  (<= count max-count))

(defn handle-dissonance
  "Return a function which can be used to control dissonance values in
  one segment of the piece."
  [{:keys [max-count diss-params max-lingering] :as m}]
  (s/validate ms/DissonanceFnParams m)
  (fn [events event]
    (->> events
         (forward-time)
         (join-events event)
         (filter event-count-ok?)
         (filter-by-dissonance-value diss-params))))

(defn extend-events
  [diss-fn-params events]
  (-> (handle-dissonance diss-fn-params)
      (reductions events)
      ))

(defn extend-phrases
  [diss-fn-params coll phrases]
  (if (empty? phrases)
    coll
    (let [next_ (map #(join-events % (last coll))
                     (first phrases))
          next_2 (map-indexed (fn [i x]
                               (map (fn [y]
                                      (update y :count (partial + i))))
                               x)
                             next_)]
      ;; TODO: if phrase is not valid, segment.
      (println next_)
      (extend-phrases diss-fn-params
                      ;; (concat coll next_2)
                      (concat coll next_2)
                      (rest phrases)))))


      ;; (rest)))

;;-----------------------------------------------------------------------------
;; Modify durations.

(s/defn pairwise-mod
  :- [ms/Chord]
  [chords :- [ms/Chord]
   match-fns :- [s/Any]
   coll :- [ms/Chord]]
  (let [pair (take 2 chords)]
    (cond (empty? pair)
          coll
          :else
          (let [result (drop-while
                        nil?
                        (map #(% pair) match-fns))]
            (if (empty? result)
              (pairwise-mod (rest chords)
                            match-fns
                            (concat coll [(first chords)]))
              (pairwise-mod (drop 2 chords)
                            match-fns
                            (concat coll (into [] (first result)))))))))

(s/defn modify-durations
  :- [ms/Chord]
  [chords :- [ms/Chord]
   mod-fns :- [s/Any]]
  (pairwise-mod chords mod-fns []))
