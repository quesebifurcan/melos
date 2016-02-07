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

(s/set-fn-validation! true)

(defn unfold-parameters
  [m]
  (->> m
       (iterate (partial functor/fmap utils/rotate))
       (map (partial functor/fmap first))))

(s/defn upper
  ;; :- [ms/Phrase]
  [part segmentation]
  (->> {:pitch (map (fn [x] [x]) (range 11))
        :is-rest? (concat (repeat 10 false)
                          [true])
        :part [part]
        :duration (concat (repeat 9 1/4)
                          [3/4 2/4])}
       unfold-parameters
       (map utils/make-chord-from-pitch-vector-params)
       (map #(s/validate ms/Chord %))
       cycle
       (utils/cyclic-partition segmentation)
       (map #(s/validate ms/Phrase %))
       ))

(defn -main
  [output-path analysis-dir config-file-path]
  (utils/export-to-json output-path
                        (compose-score/new-compose)))

(def diss-params
  {:check (fn [events]
            (<= (chord/scaled-dissonance-value (map :pitch events))
                (chord/scaled-dissonance-value [0 2 4 5])))})

(def initial-state
  {:diss-fn-params {:max-count 100
                    :max-lingering 300
                    :diss-params diss-params}
   :tempo 200
   :measures [measures/measure-3]
   :pre []
   :post []
   :part-names [:upper :lower :ped]})

(let [melody-sources (atom {:upper (upper :upper [3])
                            :lower (upper :lower [2])
                            :ped (upper :ped [1])})
      melodic-indices (->> [:upper :lower :ped]
                           (cycle)
                           (take 70))]
  (->>
   ;; Collect phrases
   (chord-seq/collect-events-in-segment
    melodic-indices
    melody-sources)
   (s/validate [ms/Phrase])
   ;; Extend phrases
   (chord-seq/extend-phrases {:max-count 100
                              :max-lingering 300
                              :diss-params [0 2 4 5]}
                             [])
   ;; Filter phrases
   (partition-by (comp :phrase-end first))
   (filter #(>= (count %) 5))
   (s/validate [ms/Phrase])
   ;; Compose part
   (map (fn [phrase]
          (->> phrase
               (chord-seq/merge-horizontally)
               (rhythm-tree/extend-last 4/4)
               (rhythm-tree/make-r-tree [measures/measure-4])
               (part/compose-part 200 [:upper :lower :ped]))))
   (utils/export-to-json "/Users/fred/projects/music/compositions/2015/organ/output/score.json")
   ))

;; TODO: unfold-parameters correctly.

;; (let [a [1 2 3 4 :end 5 6 7 8 :end 9]]
;;   (partition-by #(= :end %) a))
