(ns melos.tools.modify-durations
  (:require [schema.core :as s]
            [melos.tools.schemata :as ms]))

;; "Horizontally" merge adjacent events.

;; Set durations of "unexpected" sequences.

;; (require '[melos.tools.dissonance-calculator :refer [scaled-dissonance-value]])

(defn match-events
  [events match]
  (let [{:keys [fn_ result]} match]
    (if (and (every? #(:allow-extension? %) (flatten events))
             (fn_ events))
      result)))

(defn get-dur-2
  [{:keys [events matches] :as state}]
  (let [pair (take 2 events)]

    (cond
     (empty? pair)
     state
      (= (count pair) 1)
          (update-in state [:result] conj nil)

          :else
          (let [match (drop-while #(not (match-events pair %)) matches)
                result (:result (first match))
                cnt ((fn [x] (if (nil? x) 1 2)) result)]
            (recur (-> state
                       (update-in [:events] (partial drop cnt))
                       (update-in [:result] conj result)))))))

(require '[melos.tools.dissonance-calculator :refer [scaled-dissonance-value]])

(defn dissonant-melody-movements
  [pairs coll]
  (if (empty? pairs)
    coll
  (let [pair (first pairs)
        melodic-notes (mapcat
                       (fn [x] (filter #(= (:count %) 0) x))
                       pair)
        melodic-notes (filter :dissonance-contributor? melodic-notes)
        melodic-notes (partition-by :part melodic-notes)
        ;; TODO: sort by descending pitch.
        melodic-notes (map
                       (fn [x]
                         (first (filter number? (map :pitch x))))
                           melodic-notes)
        pitches melodic-notes
        ]
    (if (> (scaled-dissonance-value pitches) 1.6)
      (recur (rest (rest pairs)) (concat coll [6/4 6/4]))
      (recur (rest pairs) (concat coll [nil]))))))

(defn get-durations
  [pairs]
  (dissonant-melody-movements pairs [])
   )

(defn modify-durations
  [events]
  (let [pairs (partition 2 1 events)
        durations (get-durations pairs)]
    (map (fn [event dur]
           (if (nil? dur)
             event
             (-> event
                 ((fn [x] (map #(assoc % :duration dur) x)))
                 ((fn [x] (map #(assoc % :delta-dur dur) x))))))
         events
         durations)))

(defn can-merge?
  [curr next]
  (let [old-curr (filter #(> (:count %) 0) next)
        news (filter #(= (:count %) 0) next)]
    (and (= (count curr) (count old-curr))
         (every? #(:merge-left? %) news)
         (every? #(:merge-right? %) old-curr))))

(defn merge-elts
  [a b]
  (let [melodic-notes (filter #(= (:count %) 0) b)]
    (concat a melodic-notes)))

(defn maybe-merge
  ([events]
   (if (seq events)
     (maybe-merge (first events)
                  (rest events))))
  ([head events]
   (cond (empty? events)
         (list head)
         (can-merge? head (first events))
         (maybe-merge (merge-elts head
                                  (first events))
                      (rest events))
         :else
         (cons head (maybe-merge events)))))

(defn maybe-mod-a
  [[a b]]
  (if (< (:a a) (:a b))
    (let [durs [23 43]]
      (map (fn [event dur] (assoc event :dur dur))
           [a b]
           durs))))

(defn dissonant-melody-movement-mod
  [pair]
  (let [melodic-notes (mapcat
                       (fn [x] (filter #(= (:count %) 0) x))
                       pair)
        melodic-notes (filter :dissonance-contributor?
                              melodic-notes)
        melodic-notes (partition-by :part melodic-notes)
        melodic-notes (map
                       (fn [x]
                         (first (filter number? (map :pitch x))))
                           melodic-notes)]
    (if (> (scaled-dissonance-value melodic-notes) 1.6)
      (map (fn [events dur]
             (map (fn [event]
                    (assoc event
                           :delta-dur dur
                           :duration dur))
                  events))
           pair
           [3/4 3/4]))))

(require '[melos.tools.make-note :refer [make-note]])

;; (dissonant-melody-movement-mod
;;  [(map make-note [{:pitch 0 :part :upper}])
;;   (map make-note [{:pitch 1 :part :lower}])])

(defn pairwise-mod
  [events tests coll]
  (let [pair (take 2 events)]
    (cond (empty? pair)
          coll
          ;; (= (count pair) 1)
          ;; (conj coll (first pair))
          :else
          (let [result (drop-while
                        nil?
                        (map #(% pair) tests))]
            (if (empty? result)
              (pairwise-mod (rest events)
                            tests
                            (concat coll [(first events)]))
              (pairwise-mod (drop 2 events)
                            tests
                            (concat coll (into [] (first result)))))))))

(require '[melos.tools.schemata :as ms])
(require '[schema.core :as s])

;; (defn extend-last
;;   [events]
;;   (let [last-event (last events)
;;         melodic-dur (:delta-dur
;;                      (first (filter #(= (:count %) 0) last-event)))
;;         new-last (map (map (fn [x]
;;                                (assoc x :duration 3/4))
;;                              events)
;;         ]
;;     (concat (butlast events)
;;             [last-event])))

(s/defn modify-durations
  :- [ms/VerticalMoment]
  [events :- [ms/VerticalMoment]]
  (->> (pairwise-mod events
                [dissonant-melody-movement-mod]
                [])
       ;; (extend-last)))
       ))

;; (pairwise-mod events []))

;; (let [events [{:a 123}
;;               {:a 256}
;;               {:a 10}
;;               {:a 8}
;;               {:a 1000}]
;;       fns [maybe-mod-a]]
;;   (pairwise-mod events
;;                 fns))
