(ns melos.score.main
  (:require clojure.edn
            [clojure.math.combinatorics :as combinatorics]
            [clojure.set :as set]
            [schema.core :as s]
            [melos.lib
             [chord :as chord]
             [chord-seq :as chord-seq]
             [note :as note]
             [part :as part]
             [rhythm-tree :as rhythm-tree]
             [schemas :as ms]
             [utils :as utils]]
            [melos.score.materials
             [measures :as measures]]))

;; There is a lot of schema validation in the lib namespaces.
;; Setting this to false significantly improves performance.
(s/set-fn-validation! false)

(defn chromatic
  [part segmentation transposition step-count]
  (->> {:pitch (->> [[0] [3] [0] [3] [5]]
                    (utils/transpose-all transposition))
        :part [part]
        :merge-left? [true]
        :merge-right? [true]
        :notation [{:registration "A"}]
        :duration [1/4]}
       utils/unfold-parameters
       (map utils/make-chord-from-pitch-vector-params)
       (map #(s/validate ms/Chord %))
       cycle
       (utils/cyclic-partition segmentation)
       (map #(s/validate ms/Phrase %))))

(defn -main
  [output-path]
  (let [melody-sources (atom {:upper (chromatic :upper [2] -5 11)
                              :lower (chromatic :lower [2 1 1] -9 10)
                              :ped (chromatic :ped [1] -13 7)})
        diss-fn-params {:max-count 100
                        :max-lingering 300
                        :diss-params [0 2 4 5]}
        melodic-indices (->> [:upper :lower :ped]
                             (cycle)
                             (take 20))]
    (->> (chord-seq/collect-events-in-segment
          melodic-indices
          melody-sources)
         (s/validate [ms/Phrase])
         ;; Extend phrases
         (chord-seq/extend-phrases diss-fn-params [])
         ;; Filter phrases
         (utils/partition-groups (comp :phrase-end first) [] [])
         (s/validate [ms/Phrase])
         ((fn [x]
            (do (println (count x))
                x)))
         ;; Compose part
         (map (fn [tempo phrase]
                (->> phrase
                     chord-seq/merge-horizontally
                     (rhythm-tree/extend-last 4/4)
                     (rhythm-tree/make-r-tree [measures/measure-4])
                     (part/compose-part tempo [:upper :lower :ped])
                     ))
              (cycle [120]))
         (utils/export-to-json output-path))))
