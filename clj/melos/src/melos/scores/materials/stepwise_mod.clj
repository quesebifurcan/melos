(ns melos.scores.materials.stepwise-mod
  (:require [melos.tools.chord.dissonance-calculator :refer [scaled-dissonance-value]]
            [schema.core :as s]
            [melos.tools.schemata :as ms]))

(defn sustain-dissonant-vertical-moment
  [vertical-moment]
  (let [pitches (map :pitch vertical-moment)]
    (if (> (scaled-dissonance-value pitches)
           (scaled-dissonance-value [0 1]))
      (map #(assoc % :duration 8/4) vertical-moment)
      vertical-moment)))

(defn sustain-dissonant-vertical-moments
  [events]
  (map sustain-dissonant-vertical-moment events))

(defn dissonance->durations
  [vertical-moment]
  (let [pitches (map :pitch vertical-moment)
        diss-value (scaled-dissonance-value pitches)
        diss->dur (cond
                    (< diss-value
                       (scaled-dissonance-value [0 7]))
                    6/4
                    (< diss-value
                       (scaled-dissonance-value [0 4]))
                    4/4
                    (< diss-value
                       (scaled-dissonance-value [0 2 4]))
                    3/4
                    (< diss-value
                       (scaled-dissonance-value [0 1 2 4]))
                    2/4
                    :else
                    4/4)]
    diss->dur))

(defn dissonance->durations-mapping
  [events]
  (map (fn [vertical-moment]
         (let [dur (dissonance->durations vertical-moment)]
           (map #(assoc % :duration dur) vertical-moment)))
       events))

(defn melodic-pitch-class-mapping
  [vertical-moment]
  (let [melodic-pitch-class (rem (+ 60 (:pitch (first (sort-by :count vertical-moment)))) 12)]
    (cond (contains? #{9 11 4} melodic-pitch-class)
          1/4
          (contains? #{1 6 7} melodic-pitch-class)
          2/4
          :else
          3/4)))

(defn apply-melodic-pitch-class-mapping
  [events]
  (map (fn [vertical-moment]
         (let [dur (melodic-pitch-class-mapping vertical-moment)]
           (map #(assoc % :duration dur) vertical-moment)))
       events))

(defn get-melodic-pitch
  [vertical-moment]
  (:pitch (first (filter #(= (:count %) 0) vertical-moment))))

(defn get-duration-from-interval
  [[a b]]
  (let [interval (Math/abs (- a b))]
    (cond (<= interval 2)
          1/4
          (<= interval 6)
          2/4
          :else
          3/4)))

(defn apply-durations
  [events]
  (let [melodic-pitches (map get-melodic-pitch events)
        intervals (partition 2 1 melodic-pitches)
        durations (map get-duration-from-interval intervals)]
    ;; (doall (map (fn [x] (println x)) durations))
    ;; (assert (not (contains? (set durations) nil)))
    ;; events))
  (map (fn [vertical-moment dur]
           (map #(assoc % :duration dur) vertical-moment))
       events
       durations)))

(require '[melos.tools.make-note :refer [make-note]])

(defn average-pitch
  [vertical-moments]
  (let [pitches (map :pitch (flatten vertical-moments))]
    (/ (apply + pitches)
       (count pitches))))

;; (defn partition-by-count
;;   [events]
;;   ;; TODO: why always two notes in the beginning?
;;   (->> (map (fn [[x y z] a]
;;               (if (or (> (count y) (count x))
;;                       (>= (count x) 3))
;;                 a))
;;             (partition 3 1 events)
;;             events)
;;        (partition-by nil?)
;;        (filter (complement (comp nil? first)))))

(defn acceptable-succession?
  [prev curr]
  (or (> curr prev)
      (and (>= curr 3)
           (>= prev 3))))

(defn partition-by-count
  [prev-count curr coll xs]
  (if (empty? xs)
    (conj coll curr)
    (let [head (first xs)
          head-count (count head)]
      (cond (acceptable-succession? prev-count
                                    head-count)
            (recur head-count
                   (conj curr head)
                   coll
                   (rest xs))
            :else
            (recur 0
                   []
                   (conj coll curr)
                   (rest xs))))))

(defn distinct-by
    "Returns a lazy sequence of the elements of coll, removing any elements that
    return duplicate values when passed to a function f."
    [f coll]
    (let [step (fn step [xs seen]
                 (lazy-seq
                  ((fn [[x :as xs] seen]
                     (when-let [s (seq xs)]
                       (let [fx (f x)]
                         (if (contains? seen fx)
                           (recur (rest s) seen)
                           (cons x (step (rest s) (conj seen fx)))))))
                   xs seen)))]
      (step coll #{})))

(s/defn maybe-split-vertical-moment
  :- [ms/VerticalMoment]
  [vertical-moment]
  (if (> (count vertical-moment) 1)
    (s/validate [ms/VerticalMoment]
                (->> (map (fn [x] (assoc x :count 0 :duration 1/4))
                          (sort-by :count vertical-moment))
                     (reductions conj [])
                     (rest)))
    (s/validate [ms/VerticalMoment]
                [vertical-moment])))

(defn sort-continuous
  [events]
  (let [sections (->> (partition-by-count 0 [] [] events)
                      (filter #(>= (count %) 5))
                      (sort-by average-pitch)
                      (distinct-by (fn [vm] (map (juxt :pitch :duration) vm)))
                      (into []))]
    (->> (mapcat (fn [x]
                   (concat
                    (maybe-split-vertical-moment (first x))
                    (butlast (rest x))
                    [(map (fn [y] (assoc y :duration 4/4))
                           (last x))]
                    ;; [[(make-note {:part :upper :is-rest? true})]]
                    ))
                 sections))))

(partition-by-count
 0
 []
 []
 [[1 1 1] [1] [1 1 1] [1] [1 1] [1 1 1] [1 1 1] [1 1 1] [1] [1 1 1]])


