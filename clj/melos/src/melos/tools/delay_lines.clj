(ns melos.tools.delay-lines
  (:require [clojure.math.combinatorics :as combinatorics]
            [clojure.math.numeric-tower :as math]
            [melos.tools.dissonance-calculator :as diss-calc]))

(defn dissonance-contributor?
  [x]
  (number? x))

(defn- consonant?
  "Given a maximum allowed degree of dissonance (*limit*), test if
  *vertical-moment* is consonant."
  [vertical-moment limit]
  (if (empty? vertical-moment)
    true
    ;; (do
    ;;   (println
    ;; (let [pitches (map :pitch vertical-moment)]
    ;;   (diss-calc/scaled-dissonance-value pitches)))
    (let [pitches (filter dissonance-contributor? (map :pitch
                                                       (filter #(:dissonance-contributor? %)
                                                               vertical-moment)))]
      (<= (diss-calc/scaled-dissonance-value pitches)
          limit))))

(defn- dissonance-value
  "The dissonance value of a given vertical-moment."
  [vertical-moment]
  (if (<= (count vertical-moment) 1)
    0
    (let [pitches (filter dissonance-contributor? (map :pitch (filter #(:dissonance-contributor? %)
                                                                        vertical-moment)))]
      (diss-calc/scaled-dissonance-value pitches))))

(defn- zero-count?
  "Test if *event* is the most recently added."
  [event]
  (= 0 (:count event)))

(defn- contains-zero-count
  "Test if vertical moment contains the most recently added event(s)."
  [vertical-moment]
    (some #(= % 0) (map :count vertical-moment)))

(defn- part-count-ok
  "Given a maximum part-count *limit*, check if *vertical-moment* is
  ok."
  [vertical-moment part limit]
  (->> vertical-moment
       (map part)
       (count)
       (>= limit)))

(defn- find-best-candidate
  "Helper function for filter-by-count. Finds the least dissonant
  subset of events."
  [f events limit]
  (let [candidates (combinatorics/combinations events (- (count events) 1))]
    (->> candidates
         (filter contains-zero-count)
         (sort-by dissonance-value)
         (first)
         (f limit))))

;; FIXME: steady stream of NullPointerExceptions.
(defn- filter-parts-by-count [limit events]
  (->> events
       (sort-by :part)
       (partition-by :part)
       (mapcat #(take limit %))))

(defn- filter-by-count
  "If *events* contain more elements than *limit* allows, recursively
  reduce the number of elements until the collection is acceptable."
  [limit events]
  (if (<= (count events) limit)
    events
    (find-best-candidate filter-by-count events limit)))

(defn- filter-by-count-aggressive
  "Same as filter-by-count, except that when *events* contain more
  elements than *limit* allows, only the most recently added elements
  are kept. The musical effect is quite interesting -- more sudden and
  radical shifts between consonance and dissonance."
  [limit events]
  (if (<= (count events) limit)
    events
    (filter zero-count? events)))

(defn- filter-by-time-in-vertical-moment
  "Limit the amount of time an event can be sustained."
  [limit events]
  (filter #(< (:count %) limit) events))

(defn- all-parts-present?
  "Are all *part-names* present in *events*?"
  [part-names events]
  (= (set part-names) (set (map :part events))))

(defn- best-part-match
  "If *candidates* (a subset of *events*) contains all parts also
  present in *events*, return that result. This is the \"preferred\"
  outcome. Otherwise, return *candidates*."
  [events candidates]
  (let [result (filter (partial all-parts-present?
                                (map :part events))
                       candidates)]
    (if (empty? result)
      candidates
      result)))

(defn- total-count
  "The sum of all :counts in *vertical-moment*. Indicates the average
  \"age\" of *vertical-moment*. This value is used for ranking
  different options. Younger generations are preferred (improves
  melodic continuity)."
  [vertical-moment]
  (apply + (map :count vertical-moment)))

;; (defn print-wait [value]
;;   (do (println value)
;;       (Thread/sleep 3000)
;;       value))
  
(defn- filter-by-dissonance-value
  "If *events* can be considered consonant, return *events*.
  Otherwise, filter *events* recursively until collection is below the
  dissonance threshold *limit*."
  [limit events]
  (let [
          grouped-events (->> events
                              (sort-by :group)
                              (partition-by :group))
        ]
  (if (or (< (count grouped-events) 2)
          (consonant? events limit))
    events
    (let [
          candidates (combinatorics/combinations
                      grouped-events
                      (- (count grouped-events) 1))
          candidates (->> candidates
                          (map flatten)
                          (filter contains-zero-count)
                          (best-part-match events)
                          (sort-by total-count)
                          (first)
                          )
          ]
      (if (empty? candidates)
        ;; If no candidates are valid, return a vector with the most
        ;; recently added events.
        (filter #(= 0 (:count %)) events)
        (recur limit candidates))))))

(defn- forward-time
  "Increment :count of all events."
  [events]
  (->> events
       (map #(update-in % [:count] inc))
       (filter :allow-extension?)))

(defn distinct-event?
  "Are events distinct? TODO: more elegance."
  [a b]
  (if (not (= (:group a) (:group b)))
    true
    (not (and (apply = (map :pitch [a b]))
              (apply = (map :part [a b]))))))

(defn- remove-duplicated-events [events event]
  (filter #(distinct-event? % event) events))

(defn- join-events
  "Concat *new-event* and *events*, ensuring that all events are
  distinct."
  [new-event events]
  (let [deduped (remove-duplicated-events events new-event)]
    (concat deduped new-event)))

(defn filter-parts-by-count [part-counts events]
  (mapcat (fn [[part-name limit]]
            (filter-by-count-aggressive
             limit
             (filter #(= (:part %) part-name) events)))
          part-counts))

(defn handle-dissonance
  "Return a function which can be used to control dissonance values in
  one segment of the piece."
  [{:keys [max-count part-count part-counts
           diss-value max-lingering]}]
  (fn [events event]
    (->> events
         (forward-time)
         (join-events event)
         ;; TODO: activate either filter-by-count-aggressive or
         ;; filter-parts-by-count.
         ;; (filter-by-count-aggressive max-count)

         (filter-parts-by-count part-counts)
         (filter-by-time-in-vertical-moment max-lingering)
         (filter-by-dissonance-value diss-value)

         ;; ((fn [x]
         ;;    (do (println x)
         ;;        x)))
         )))
