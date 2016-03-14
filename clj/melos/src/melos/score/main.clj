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
;; Setting this to false significantly improves performance.
(s/set-fn-validation! false)

;; (def measure
;;   (measure-util/parse-rtm-tree-node
;;    (measure-util/stretch-tree [4 4] 1 [[1 0] [0 1] [0] [0]])))

(def measure
  (measure-util/parse-rtm-tree-node
   (measure-util/stretch-tree [4 4] 0 [[0] [0] [0] [0]])))

(defn segment-melody
  [xs]
  (->> xs
       (partition-by #(= % :end))
       (take-nth 2)))

(defn chromatic
  [part segmentation transposition step-count]
  (let [pitches [0 [0 12] :end [10 12] 12 10 :end [3 10] :end]
        real-pitches (filter #(not (= :end %)) pitches)
        segmentation (->> pitches segment-melody (map count))]
    (->> {:pitch (->> real-pitches
                      (map (fn [x] (if (number? x) [x] x)))
                      (utils/transpose-all transposition))
        :part [part]
        ;; :merge-left? [true]
        ;; :merge-right? [true]
        :notation [{:registration "A"}]
        :duration (map (fn [x] (if (= x [3 10]) 3/4 1/4)) real-pitches)}
       utils/unfold-parameters
       (map utils/make-chord-from-pitch-vector-params)
       (map #(s/validate ms/Chord %))
       cycle
       (utils/cyclic-partition segmentation)
       (map #(s/validate ms/Phrase %)))))

(defn -main
  [output-path]
  (let [melody-sources (atom {:upper (chromatic :upper [2] 0 11)
                              :lower (chromatic :lower [2 1 1] -7 10)
                              :ped (chromatic :ped [1] -12 7)})
        diss-fn-params {:max-count 100
                        :max-lingering 300
                        :diss-params [0 1 2]}
        melodic-indices (->> [:upper :lower :ped]
                             (cycle)
                             (take 200))]
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
                     (rhythm-tree/make-r-tree [measure])
                     (part/compose-part tempo [:upper :lower :ped])
                     ))
              (cycle [100]))
         (utils/export-to-json output-path))))

(defn chromatic-2
  [part segmentation transposition step-count]
  (->> {:pitch (->> [[2] [10] [2] [10] [2] [10] [9]]
                    (utils/transpose-all transposition))
        :part [part]
        :merge-left? [true]
        :merge-right? [true]
        :notation [{:registration "B"}]
        :duration [1/4]}
       utils/unfold-parameters
       (map utils/make-chord-from-pitch-vector-params)
       (map #(s/validate ms/Chord %))
       cycle
       (utils/cyclic-partition segmentation)
       (map #(s/validate ms/Phrase %))))

(defn main-2
  [output-path]
  (let [melody-sources (atom {:upper (chromatic-2 :upper [2] -5 11)
                              :lower (chromatic-2 :lower [2 1 1] -9 10)
                              :ped (chromatic-2 :ped [1] -13 7)})
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
                     (rhythm-tree/make-r-tree [measure])
                     (part/compose-part tempo [:upper :lower :ped])))
              (cycle [180]))
         (utils/export-to-json output-path))))
