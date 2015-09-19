(ns score.compose-score
  (:require [clojure.math.combinatorics :as combinatorics]
            [melos
             [note :as note]
             [chord :as chord]
             [graphs :as graphs]
             [chord-seq :as chord-seq]
             [rhythm-tree :as rhythm-tree]
             [part :as part]
             [schemas :as ms]
             [params :as params]
             [utils :as utils]]
            [schema.core :as s]
            [score.combinations :as combinations]
            [score.materials
             [measures :as measures]]))

;;-----------------------------------------------------------------------------
;; SCORE

(defn compose-segment
  [{:keys [events
           diss-fn-params
           pre
           post
           measures
           tempo
           part-names]}]
  (->> events
       ((apply comp pre))
       (chord-seq/extend-events diss-fn-params)
       (chord-seq/merge-horizontally)
       ((apply comp post))
       (rhythm-tree/make-r-tree measures)
       (part/compose-part tempo part-names)))

(defn make-overlaps [pitches]
  (->> pitches
       (partition 2 1)
       (mapcat (fn [[x y]]
                 [[x] [x y]]))))

(defn diatonic-ped
  [part-name transposition]
  {:pitch (->> [0 2 4 5 7 9]
               (utils/transpose transposition)
               (make-overlaps))
   :part [part-name]
   :fn utils/make-chord-from-pitch-vector-params
   :partition (partial utils/cyclic-partition [2])
   :max-part-count [2]
   :duration [1/4]})

(defn upper
  [part-name transposition]
  {:pitch [[0] [0 7] [0 7 12] [0 7 12] [7 12] [12] [-1] [-1 0] [-1 0 2]]
   :part [part-name]
   :fn utils/make-chord-from-pitch-vector-params
   :partition (partial utils/cyclic-partition [1])
   :max-part-count [3]
   :merge-left? [true]
   :merge-right? [true]
   :duration [1/4]})

(defn diatonic-ped
  [part-name transposition]
  {:pitch (->> (range 10 0 -1)
               (utils/transpose transposition)
               (map (fn [x] [x])))
               ;; (make-overlaps))
   :part [part-name]
   :fn utils/make-chord-from-pitch-vector-params
   :partition (partial utils/cyclic-partition [2])
   :max-part-count [1]
   ;; :max-lingering [200]
   :merge-left? [true]
   :merge-right? [true]
   :duration [1/4]})

(defn diatonic-ped-2
  [part-name transposition]
  {:pitch (->> (range 8 0 -1)
               (utils/transpose transposition)
               (map (fn [x] [x])))
   :part [part-name]
   :fn utils/make-chord-from-pitch-vector-params
   :partition (partial utils/cyclic-partition [1])
   :max-part-count [1]
   :merge-left? [true]
   :merge-right? [true]
   :duration [1/4]})

(def chord-seq
  (let [melodic-indices (take 50 (cycle [:upper/a :lower/a :ped/a]))
        fn_ (comp utils/unfold-events diatonic-ped)
        melody-sources-atom (atom {:lower/a (fn_ :lower -7)
                                   :upper/a (utils/unfold-events (upper :upper -3))
                                   ;; :upper/a (upper)
                                   :ped/a (utils/unfold-events (diatonic-ped-2 :ped -20))})]
    (chord-seq/collect-events-in-segment melodic-indices
                               melody-sources-atom)))

(def diss-params
  {:check (fn [events]
            (<= (chord/scaled-dissonance-value (map :pitch events))
                (chord/scaled-dissonance-value [0 2 4 5])))})

(defn initial-state
  []
  {:events chord-seq
   :diss-fn-params {:max-count 100
                    :max-lingering 300
                    ;; TODO: pass on diss-value
                    :diss-params diss-params}
   :tempo 120
   :measures [measures/measure-4]
   :pre []
   ;; :post [(partial rtm/extend-last 7/4)]
   :post []
   :part-names [:upper :lower :ped]})

(defn compose []
  (->> (compose-segment
        (initial-state))
       (conj [])))

