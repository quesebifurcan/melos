(ns melos.score.main
  (:require [melos.lib.utils :as utils]
            [melos.score.compose-score :as compose-score]
            clojure.edn
            [clojure.math.combinatorics :as combinatorics]
            [clojure.set :as set]
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

(s/defn upper
  :- [ms/Phrase]
  [part segmentation]
  (->> {:pitch (map (fn [x] [x]) (range 10))
        :part [part]
        :duration [1/4]}
       combinations/unfold-parameters
       (map utils/make-chord-from-pitch-vector-params)
       (map #(s/validate ms/Chord %))
       cycle
       (utils/cyclic-partition segmentation)
       (map #(s/validate ms/Phrase %))
       (take 10)))

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
                    ;; TODO: pass on diss-value
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
                           (take 10))]
  (->> (chord-seq/collect-events-in-segment
        melodic-indices
        melody-sources)
       ;; Work with phrases, not with chords?
       ;; Extend phrases returns list of phrases
       ;; which can be filtered and sorted.
       ;; TODO: Multiple segments?
       (s/validate [ms/Phrase])
       (chord-seq/extend-phrases {:max-count 100
                                 :max-lingering 300
                                 :diss-params diss-params}
                                [])
       (chord-seq/merge-horizontally)
       (rhythm-tree/make-r-tree [measures/measure-3])
       (part/compose-part 200 [:upper :lower :ped])
       ((fn [x] [x]))
       (utils/export-to-json "/Users/fred/projects/music/compositions/2015/organ/output/score.json")
       ))
