(ns melos.score.main
  (:require [melos.lib.utils :as utils]
            [melos.score.compose-score :as compose-score]
            clojure.edn
            [clojure.math.combinatorics :as combinatorics]
            [clojure.set :as set]
            [clojure.algo.generic.functor :as functor]
            [melos.lib
             [chord :as chord]
             [chord-seq :as chord-seq]
             [note :as note]
             [params :as params]
             [part :as part]
             [rhythm-tree :as rhythm-tree]
             [schemas :as ms]
             [utils :as utils]]
            [melos.score
             [group-c :as group-c]
             [combinations :as combinations]]
            [melos.score.materials
             [measures :as measures]
             [stepwise-mod :as stepwise-mod]]
            [progressbar.core :refer [progressbar]]
            [schema.core :as s]))

(defn partition-groups
  [f curr coll l]
  (if (empty? l)
    (if (empty? curr)
      coll
      (concat coll [curr]))
    (let [nxt (first l)]
      (if (f nxt)
        (partition-groups f
                          []
                          (concat coll
                                  [(concat curr
                                           [nxt])])
                          (rest l))
        (partition-groups f
                          (concat curr [nxt])
                          coll
                          (rest l))))))

(defn transpose-all
  [step forms]
  (clojure.walk/postwalk
   (fn [form]
     (cond (number? form)
           (+ form step)
           :else
           form))
   forms))

(s/set-fn-validation! false)

(defn unfold-parameters
  [m]
  (->> m
       (iterate (partial functor/fmap utils/rotate))
       (map (partial functor/fmap first))))

(s/defn chromatic
  [part segmentation transposition]
  (->> {:pitch (->> (map (fn [x] [x])  [0 2 4 5 7 9])
                    (transpose-all transposition))
        :is-rest? (concat (repeat 10 false)
                          [true])
        :part [part]
        :merge-left? [true]
        :merge-right? [true]
        :duration (concat (repeat 9 1/4)
                          [3/4 1/4])}
       unfold-parameters
       (map utils/make-chord-from-pitch-vector-params)
       (map #(s/validate ms/Chord %))
       cycle
       (utils/cyclic-partition segmentation)
       (map #(s/validate ms/Phrase %))
       ))

(s/defn chords
  ;; :- [ms/Phrase]
  [part segmentation transposition]
  (->> {:pitch (->> [[0] [0 2] [0 2 5] [2 5] [5]]
                    (transpose-all transposition))
        :part [part]
        :duration (concat (repeat 9 1/4)
                          [1/4 1/4])}
       unfold-parameters
       (map utils/make-chord-from-pitch-vector-params)
       (map #(s/validate ms/Chord %))
       cycle
       (utils/cyclic-partition segmentation)
       (map #(s/validate ms/Phrase %))
       ))

(s/defn coll-part-counts-map
  [chord :- ms/Chord]
  (let [partitioned-events (->> chord
                                (sort-by :part)
                                (partition-by :part))]
    (zipmap (map (comp :part first) partitioned-events)
            (map (comp :max-part-count first) partitioned-events))))

(defn -main
  [output-path analysis-dir config-file-path]
  (let [melody-sources (atom {:upper (chromatic :upper [3 1 1] 0)
                              :lower (chromatic :lower [2] -3)
                              :ped (chromatic :ped [1] -13)})
        melodic-indices (->> [:upper :lower :ped :upper :lower]
                             (cycle)
                             (take 10))]
    (->> (chord-seq/collect-events-in-segment
          melodic-indices
          melody-sources)
         (s/validate [ms/Phrase])
         ;; Extend phrases
         (chord-seq/extend-phrases {:max-count 100
                                    :max-lingering 300
                                    :diss-params [0 1 2]}
                                   [])
         ;; Filter phrases
         (partition-groups (comp :phrase-end first)
                           []
                           [])
         ;; (filter #(>= (count %) 5))
         ;; (rest)
         (s/validate [ms/Phrase])
         ;; Compose part
         (map (fn [phrase]
                (->> phrase
                     (chord-seq/merge-horizontally)
                     ;; (rhythm-tree/extend-last 4/4)
                     (rhythm-tree/make-r-tree [measures/measure-4])
                     (part/compose-part 200 [:upper :lower :ped]))))
         (utils/export-to-json "/Users/fred/projects/music/compositions/2015/organ/output/score.json")
         )))

;; (reductions (fn [x y]
;;           (let [prev (set (map (juxt :part :pitch) x))]
;;             (map (fn [z]
;;                    (if (contains? prev ((juxt :part :pitch) z))
;;                      (update z :count inc)
;;                      z))
;;                  y)))
;;             [

;; (defn update-counts
;;   [phrase prev coll]
;;   (if (empty? phrase)
;;     coll
;;     (let [new (first phrase)
;;           new-counts (map (fn [x]
;;                             (if (contains? prev ((juxt :part :pitch) x))
;;                               (update x :count (fn []
;;                                                  (+ 1 (prev ((juxt :part :pitch) x)))))
;;                               x))
;;                           new)]
;;       (update-counts (rest phrase)
;;                      (map (juxt :part :pitch) new-counts)
