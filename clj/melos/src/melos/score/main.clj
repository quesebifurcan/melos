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
             [measure :as measure-util]
             [utils :as utils]]))

;; There is a lot of schema validation in the lib namespaces.
;; Setting this to `false` significantly improves performance.
(s/set-fn-validation! true)

;; basic 4/4 subdivisions
(def measure:4-4
  (measure-util/parse-rtm-tree-node
   (measure-util/stretch-tree [4 4] 0 [[0] [0] [0] [0]])))

(def measure:stretched
  (measure-util/parse-rtm-tree-node
   (measure-util/stretch-tree [4 4] 1 [[1 0] [0 0] [0] [0]])))

(defn chromatic
  [part transposition]
  (let [pitches [0 2 :end 0 :end 2 0 :end 2]
        real-pitches (->> pitches
                          (filter (complement keyword?)))
        segmentation (->> pitches utils/segment-melody (map count))]
    (->> {:pitch (->> real-pitches
                      (map (fn [x] (if (number? x) [x] x)))
                      (utils/transpose-all transposition))
        :part [part]
        :merge-left? [false]
        :merge-right? [false]
        :notation [{:registration "A"}]
        :duration [1/4]}
       utils/unfold-parameters
       (map utils/make-chord-from-pitch-vector-params)
       (map #(s/validate ms/Chord %))
       cycle
       (utils/cyclic-partition segmentation)
       (map #(s/validate ms/Phrase %)))))

(defn make-score
  [output-path]
  (let [melody-sources (atom {:voice-1 (chromatic :voice-1 7)
                              :voice-2 (chromatic :voice-2 4)
                              :voice-3 (chromatic :voice-3 0)
                              :voice-4 (chromatic :voice-4 -5)
                              :voice-5 (chromatic :voice-5 -12)})
        diss-fn-params {:max-count 100
                        :max-lingering 300
                        :diss-params [0 1 2]}
        melodic-indices (->> [:voice-1
                              :voice-2
                              :voice-3
                              :voice-4
                              :voice-5
                              :voice-1
                              :voice-3
                              :voice-1
                              :voice-2]
                             (cycle)
                             (take 40))
        final-event-extensions [8/4 4/4]
        tempi [100]]
    (->> (chord-seq/collect-events-in-segment
          melodic-indices
          melody-sources)
         ;; Extend phrases
         (chord-seq/extend-phrases diss-fn-params [])
         ;; Filter phrases
         (utils/partition-groups (comp :phrase-end first) [] [])
         ;; Compose part
         (map (fn [tempo extension phrase]
                (->> phrase
                     chord-seq/merge-horizontally
                     (rhythm-tree/extend-last extension)
                     (rhythm-tree/make-r-tree [measure:stretched])
                     (part/compose-part tempo [:voice-1
                                               :voice-2
                                               :voice-3
                                               :voice-4
                                               :voice-5])))
              (cycle tempi)
              (cycle final-event-extensions))
         (utils/export-to-json output-path))))
