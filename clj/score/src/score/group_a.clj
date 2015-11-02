(ns score.group-a
  (:require [clojure.math.combinatorics :as combinatorics]
            [melos.note :refer [make-note]]
            [melos
             [utils :as utils]]))

(defn transpose-all-numbers
  [step forms]
  (clojure.walk/prewalk
   (fn [form] 
     (cond (number? form)
           (+ step form)
           :else
           form))
   forms))

(comment
  (defn upper
    [part-name transposition dur]
    {:pitch [
             ;; [0] [0 7] [0 7 14] [0 7 9 14]
             [0] [1] [2] [7] [8] [9] [1] [2] [3] [-7] [-6] [-5] [-4]
             ]
     :part [part-name]
     :fn utils/make-chord-from-pitch-vector-params
     :partition (partial utils/cyclic-partition [3 3 3 4])
     :max-part-count [4]
     ;; :merge-left? [true]
     ;; :merge-right? [true]
     :duration [1/4]}))

(defn upper
  [part-name transposition dur]
  (let [phrases [[0] [0 7] [0 7 14] [0 7 9 14]]]
    (println "phrases count:" (map count phrases))

  {:pitch [
           [0] [0 7] [0 7 14] [0 7 9 14]
           ]
   :part [part-name]
   :fn utils/make-chord-from-pitch-vector-params
   :partition (partial utils/cyclic-partition [1])
   :max-part-count [4]
   ;; :merge-left? [true]
   ;; :merge-right? [true]
   :duration [2/4]}))

(defn as-chords [& pitch] (mapv (fn [x] [x]) pitch))

(concat
 (as-chords 1 2 3 4)
 [
  [1 2 3]
  [8 9 10]
  ]
 (apply as-chords (range 2 7))
 )

;; (defn testing
;;   [part-name transposition dur]
;;     {:pitch (->> [[1] [2] [3] [4]]
;;                  [[1] [2] [3] [4] [5] [6]]
;;                  )
;;      :part [part-name]
;;      :fn utils/make-chord-from-pitch-vector-params
;;      :partition (partial utils/cyclic-partition [1])
;;      :max-part-count [4]
;;      ;; :merge-left? [true]
;;      ;; :merge-right? [true]
;;      :duration [2/4]}
;;     )

;; (defn new-phrase
;;   [props]
;;   (map make-note props))

;; (new-phrase [{:pitch 0} {:pitch 2}])

;; (upper :lower 8 1/4)

;; (let [a {:pitch [
;;                  [
;;                   [ 0 ]
;;                   [ 0 2 ]
;;                   [ 2 ]
;;                   [ 3 ]
;;                   [ 4 ]
;;                   ]
;;                  [
;;                   [ 4 ]
;;                   [ 7 ]
;;                   ]
;;                  ]
;;          :duration [
;;                     [
;;                      [ 2/4 ]
;;                      ]
;;                     ]
;;          }]
;;   a)

;; [
;;  [
;;   [ 0 ]
;;   ]
;;  ]

(defn diatonic-ped
  [pitches part-name transposition]
  {:pitch (->> pitches
               (utils/transpose transposition)
               (map (fn [x] [x])))
               ;; (make-overlaps))
   :part [part-name]
   :fn utils/make-chord-from-pitch-vector-params
   :partition (partial utils/cyclic-partition [2])
   :max-part-count [1]
   ;; :max-lingering [200]
   ;; :merge-left? [true]
   ;; :merge-right? [true]
   :duration [1/4]})

(defn diatonic-ped-2
  [part-name transposition]
  ;; {:pitch (->> (range 8 0 -1)
  {:pitch (->> (range -4 8)
               (utils/transpose transposition)
               (map (fn [x] [x])))
   :part [part-name]
   :fn utils/make-chord-from-pitch-vector-params
   :partition (partial utils/cyclic-partition [1])
   :max-part-count [1]
   ;; :merge-left? [true]
   ;; :merge-right? [true]
   :duration [1/4]})

(def materials
  {:upper
   [(utils/unfold-events (upper :upper -1 2/4))]
   :lower (map (fn [offset] (drop offset
                                  (utils/unfold-events (diatonic-ped (range 10) :lower -7))))
                                  (range 8))
   :ped (map (fn [offset] (drop offset
                                (utils/unfold-events (diatonic-ped-2 :ped -20))))
             (range 20))
   :melodic-indices [(take 20 (cycle [:upper :lower :ped]))]})
