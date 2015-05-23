(ns melos.tools.modify-durations)

;; "Horizontally" merge adjacent events.
                                
(defn can-merge?
  [curr next]
    (let [old-curr (filter #(> (:count %) 0) next)
          news (filter #(= (:count %) 0) next)]
      (and (= (count curr) (count old-curr))
           (every? #(:merge-left? %) news)
           (every? #(:merge-right? %) old-curr))))

(defn maybe-merge
  [[a b]]
  (if (can-merge? a b) nil a))

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

(def test-sequence
  [
   [
    {:pitch 1 :allow-extension? true}
    ]
   [
    {:pitch 0 :allow-extension? true}
    {:pitch 2 :allow-extension? true}
    {:pitch 4 :allow-extension? true}
    ]
   [
    {:pitch 1 :allow-extension? true}
    ]
   [
    {:pitch 2 :allow-extension? true}
    {:pitch 4 :allow-extension? true}
    ]
   [
    {:pitch 2 :allow-extension? true}
    {:pitch 4}
    {:pitch 5 :allow-extension? true}
    ]
   [
    {:pitch 1 :allow-extension? true}
    ]
   [
    {:pitch 0 :allow-extension? true}
    {:pitch 2 :allow-extension? true}
    {:pitch 4 :allow-extension? true}
    ]
   [
    {:pitch 1 :allow-extension? true}
    ]
   [
    {:pitch 2 :allow-extension? true}
    {:pitch 4 :allow-extension? true}
    ]
   [
    {:pitch 2 :allow-extension? true}
    {:pitch 4 :allow-extension? true}
    {:pitch 5 :allow-extension? false}
    ]
   [
    {:pitch 1 :allow-extension? true}
    ]
   ])

;; (defn modify-durations
;;   [events]
;;   (let [matches [
;;                  {:fn_ (fn [[a b]]
;;                          (and (>= (count a) 3)
;;                               (= (count b) 1)))
;;                   :result [7/4 7/4]}
;;                  ]
;;         durations (flatten (:result (get-dur-2 {:events events
;;                                                 :matches matches
;;                                                 :result []})))]
;;     (map (fn [event dur]
;;            (if (nil? dur)
;;              event
;;              (-> event
;;                  ((fn [x] (map #(assoc % :duration dur) x)))
;;                  ((fn [x] (map #(assoc % :delta-dur dur) x))))))
;;          events
;;          durations)))

(require '[melos.tools.dissonance-calculator :refer [scaled-dissonance-value]])

;; (defn dissonant-melody-movement?
;;   [pair]
;;   (let [melodic-notes (mapcat
;;                        (fn [x] (filter #(= (:count %) 0) x))
;;                        pair)
;;         melodic-notes (partition-by :part melodic-notes)
;;         ;; TODO: sort be descending pitch.
;;         melodic-notes (map first melodic-notes)
;;         pitches (filter number? (map :pitch melodic-notes))
;;         ]
;;     (println (count melodic-notes))
;;     (if (> (scaled-dissonance-value pitches) 1.2)
;;       6/4
;;       nil)))

(defn dissonant-melody-movements
  [pairs coll]
  (if (empty? pairs)
    coll
  (let [pair (first pairs)
        melodic-notes (mapcat
                       (fn [x] (filter #(= (:count %) 0) x))
                       pair)
        melodic-notes (partition-by :part melodic-notes)
        ;; TODO: sort by descending pitch.
        melodic-notes (map
                       (fn [x]
                         (first (filter number? (map
                                                 :pitch
                                                 x))))
                           melodic-notes)
        pitches melodic-notes
        ]
    (if (> (scaled-dissonance-value pitches) 1.6)
      (recur (rest (rest pairs)) (concat coll [6/4 6/4]))
      (recur (rest pairs) (concat coll [nil]))))))

(defn get-durations
  [pairs]
  ;; (println (dissonant-melody-movements pairs []))
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

;; (modify-durations test-sequence)

