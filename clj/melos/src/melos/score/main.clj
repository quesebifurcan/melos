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

(s/set-fn-validation! false)

(s/defn chromatic
  [part segmentation transposition]
  (->> {:pitch (->> (map (fn [x] [x]) [0 2])
                    (utils/transpose-all transposition))
        :part [part]
        :merge-left? [true]
        :merge-right? [true]
        :duration (concat (repeat 9 1/4)
                          [1/4 1/4])}
       utils/unfold-parameters
       (map utils/make-chord-from-pitch-vector-params)
       (map #(s/validate ms/Chord %))
       cycle
       (utils/cyclic-partition segmentation)
       (map #(s/validate ms/Phrase %))))

(s/defn chords
  ;; :- [ms/Phrase]
  [part segmentation transposition]
  (->> {:pitch (->> [[0] [0 2] [0 2 5] [2 5] [5]]
                    (utils/transpose-all transposition))
        :part [part]
        :duration (concat (repeat 9 1/4)
                          [1/4 1/4])}
       utils/unfold-parameters
       (map utils/make-chord-from-pitch-vector-params)
       (map #(s/validate ms/Chord %))
       cycle
       (utils/cyclic-partition segmentation)
       (map #(s/validate ms/Phrase %))))

(defn -main
  [output-path analysis-dir config-file-path]
  (let [melody-sources (atom {:upper (chromatic :upper [3] 5)
                              :lower (chromatic :lower [2] -7)
                              :ped (chromatic :ped [1] -12)})
        melodic-indices (->> [:upper :lower :ped]
                             (cycle)
                             (take 200))]
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
         (utils/partition-groups (comp :phrase-end first)
                           []
                           [])
         ;; (filter #(>= (count %) 5))
         ;; (rest)
         (s/validate [ms/Phrase])
         ;; Compose part
         (map (fn [tempo phrase]
                (->> phrase
                     ;; ((fn [x] (do (println (count x))
                     ;;              x)))
                     (chord-seq/merge-horizontally)
                     (rhythm-tree/extend-last 4/4)
                     (rhythm-tree/make-r-tree [measures/measure-4])
                     (part/compose-part tempo [:upper :lower :ped])))
              (cycle [200 130 90]))
         (utils/export-to-json "/Users/fred/projects/music/compositions/2015/organ/output/score.json")
         )))
