(ns melos.score.main
  (:require [clojure.java.shell :refer [sh]]
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
(s/set-fn-validation! false)

;; basic 4/4 subdivisions
(def measure:4-4
  (measure-util/parse-rtm-tree-node
   (measure-util/stretch-tree [4 4] 0 [[0] [0] [0] [0]])))

(def measure:stretched
  (measure-util/parse-rtm-tree-node
   (measure-util/stretch-tree [4 4] 1 [[1 0] [0 0] [0] [0]])))

(defn chromatic
  [part transposition]
  (let [pitch-groups [0 0 :end 2 0 :end 0 :end 2 0 :end 0 0 :end 2 :end]
        pitches (->> pitch-groups
                     (filter (complement keyword?)))
        segmentation (->> pitch-groups utils/segment-melody (map count))]
    (->> {:pitch (->> pitches
                      (map (fn [x] (if (number? x) [x] x)))
                      (utils/transpose-all transposition))
          :part [part]
          :is-rest? [false true false true false false true false true false]
          :merge-left? [false]
          :merge-right? [false]
          :notation [{:registration "A"}]
          :duration [1/8 1/8 1/8 1/8 1/4 1/8 1/8 1/8 1/8 1/4]}
         utils/unfold-parameters
         (map utils/make-chord-from-pitch-vector-params)
         (map #(s/validate ms/Chord %))
         cycle
         (utils/cyclic-partition segmentation)
         (map #(s/validate ms/Phrase %)))))


(defn make-score
  [output-path]
  (let [melody-sources (atom {:voice-1 (chromatic :voice-1 5)
                              ;; :voice-2 (chromatic :voice-2 2)
                              :voice-3 (chromatic :voice-3 -2)
                              ;; :voice-4 (chromatic :voice-4 -7)
                              :voice-5 (chromatic :voice-5 -16)})
        diss-fn-params {:max-count 100
                        :max-lingering 300
                        :diss-params [0 1 2]}
        melodic-indices (->> [
                              :voice-3
                              :voice-1
                              :voice-3
                              :voice-1
                              :voice-3
                              :voice-5
                              :voice-1
                              :voice-3
                              :voice-5
                              ]
                             (cycle)
                             (take 80))
        final-event-extensions [4/4 4/4]
        tempi [132]]
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
                     (rhythm-tree/make-r-tree [measure:4-4])
                     (part/compose-part tempo [:voice-1
                                               ;; :voice-2
                                               :voice-3
                                               ;; :voice-4
                                               :voice-5])))
              (cycle tempi)
              (cycle final-event-extensions))
         (utils/export-to-json output-path))))

(defn render []
  (let [json-output-path "../../output/test.json"
        pdf-output-path "../../output/score.pdf"
        python "../../env/bin/python"
        script "../../py/main.py"]
    (make-score json-output-path)
    (sh python script "--input-files" json-output-path "--score-out" pdf-output-path)
    (println (str "Rendered PDF to " pdf-output-path))))
