(ns melos.tools.segment-durations
  (:require [melos.tools.utils :refer [rotate ratio->non-reduced-ratio-vector]]))

;; ## TODO: this namespace is embarrassingly hacky ("but it works"). Clean up sooner rather than later.

(def note-values [1/16 1/8 3/16 1/4 3/8 1/2 3/4 4/4])

(defn filter-note-values [coll max-dur]
  (filter #(<= % max-dur) coll))

(defn curr-ts [m] (first (:ts m)))

(defn start-on-pulse? [m]
  (= (rem (:start m)
          (/ 1 (second (curr-ts m))))
     0))

(defn pulse [m]
  (/ 1 (second (curr-ts m))))

(defn previous-beat [m]
  (/ (quot (:start m) (pulse m)) (second (curr-ts m))))

(defn conventional-endpoint [m]
  (cond (start-on-pulse? m)
        (min (+ (:start m) (* 4 (pulse m)))
             (+ (:start m) (last (filter-note-values note-values (:dur m))))
             (/ (first (curr-ts m)) (second (curr-ts m))))
        :else
        (min (+ (previous-beat m) (pulse m))
             (+ (:start m) (last (filter-note-values note-values (:dur m)))))))

(defn conventional-dur [m]
  (- (conventional-endpoint m) (:start m)))

(defn maybe-update-ts [m]
  (if (= 0N (:start m))
    (update-in m [:ts] rotate)
    m))

(defn partially-update-state [m dur]
  (merge m {:start (rem (+ (:start m) dur) (/ (first (curr-ts m))
                                              (second (curr-ts m))))
            :dur (- (:dur m) dur)}))

(defn next-point [m]
  (let [dur (conventional-dur m)
        diff (- (+ (:start m) (:dur m))
                (+ (:start m) dur))]
    (if (= diff 0)
      (-> m
          (update-in [:result] (fn [x] (conj x dur)))
          (update-in [:start] (fn [x] (+ x dur))))
      (next-point (-> m
                      (partially-update-state dur)
                      (maybe-update-ts)
                      (update-in [:result] (fn [x] (conj x dur))))))))

(defn copy-result-to-coll [m]
  (let [result (filter #(> % 0) (:result m))]
    (update-in m [:coll] (fn [x] (conj x result)))))

(defn multiple-durs [state dur]
    (-> state
        (assoc :dur dur)
        (next-point)
        (copy-result-to-coll)
        (assoc :result [])
        ))

(defn get-segmented-durations
  [time-signatures durations]
  (let [initial-state {:ts time-signatures
                       :start 0
                       :dur 0
                       :result []
                       :coll []}]
    (->> (reduce multiple-durs initial-state durations)
         (:coll))))

(defn get-durations [events]
  (map #(:delta-dur (first %)) events))

(defn segment-vertical-moment [vertical-moment segmentation onset-increments]
  (map (fn [event]
         (map (fn [dur incr]
                (assoc event :delta-dur dur :onset (+ (:onset event) incr)))
              segmentation
              onset-increments))
       vertical-moment))
