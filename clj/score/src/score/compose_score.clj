(ns score.compose-score
  (:require [clojure.math.combinatorics :as combinatorics]
            [melos
             [note :as note]
             [chord :as chord]
             [chord-seq :as chord-seq]
             [rhythm-tree :as rhythm-tree]
             [part :as part]
             [schemas :as ms]
             [params :as params]
             [utils :as utils]]
            [schema.core :as s]
            [score.combinations :as combinations]
            [score.materials
             [measures :as measures]
             [stepwise-mod :as stepwise-mod]]))

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

(defn compose-event-seq
  [{:keys [events
           diss-fn-params
           pre
           post]}]
  (->> events
       ((apply comp pre))
       (chord-seq/extend-events diss-fn-params)
       (chord-seq/merge-horizontally)
       ((apply comp post))))

(defn compose-parts
  [measures tempo part-names event-seq]
  (let [head (stepwise-mod/maybe-split-vertical-moment 
              (first event-seq))
        event-seq-mod (concat head (rest event-seq))]
  (->> event-seq-mod
       (rhythm-tree/extend-last 7/4)
       (rhythm-tree/make-r-tree measures)
       (part/compose-part tempo part-names))))

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

(def materials
  {:upper [
           (utils/unfold-events (upper :upper -3))
           (utils/unfold-events (upper :upper 3))
           ]
   :lower [
           (utils/unfold-events (diatonic-ped :lower -7))
           (utils/unfold-events (diatonic-ped :lower -5))
           ]
   :ped [
         (utils/unfold-events (diatonic-ped-2 :ped -20))
         (utils/unfold-events (diatonic-ped-2 :ped -10))
         ]
   :melodic-indices [(take 50 (cycle [:upper :lower :ped]))]
   })

(defn make-chord-seq
  [{:keys [upper lower ped melodic-indices]}]
  (let [melody-sources-atom (atom {:lower lower
                                   :upper upper
                                   :ped ped})]
    (chord-seq/collect-events-in-segment melodic-indices
                                         melody-sources-atom)))

(def chord-seqs
  (->> materials
       (combinations/unfold-parameters)
       (map make-chord-seq)))

(def diss-params
  {:check (fn [events]
            (<= (chord/scaled-dissonance-value (map :pitch events))
                (chord/scaled-dissonance-value [0 2 4 5])))})

(defn initial-state
  [events]
  {:events events
   :diss-fn-params {:max-count 100
                    :max-lingering 300
                    ;; TODO: pass on diss-value
                    :diss-params diss-params}
   :tempo 120
   :measures [measures/measure-4]
   :pre []
   ;; :post [(partial rhythm-tree/extend-last 7/4)]
   :post []
   :part-names [:upper :lower :ped]})

(defn parts-in-chord
  [chord]
  (set (map :part chord)))

(defn part-count-sufficient?
  [minimum chord]
  (let [part-count ((comp count parts-in-chord) chord)]
    (= part-count minimum)))

(def partition-events-fn
  (partial part-count-sufficient? 3))

(defn filter-events-fn
  [events]
  (and (>= (count events) 5)
       (every? (partial part-count-sufficient? 3) events)))

(defn compose []
  (let [states (map initial-state chord-seqs)]
  (->> states
       (map compose-event-seq)
       ;; (first)
       ;; (map (partial compose-parts
       ;;               [measures/measure-4]
       ;;               120
       ;;               [:upper :lower :ped])))))
       ;; (partition-by
       ;;  (partial part-count-sufficient? 2))
       (apply concat)
       (partition-by partition-events-fn)
       (filter filter-events-fn)
       ;; (apply concat)
       (map (partial compose-parts
                     [measures/measure-4]
                     120
                     [:upper :lower :ped]))
       ;; (map (fn [x]
       ;;        (map (comp count parts-in-chord) x)))
       ;; (map (fn [x] (count x)))
       )))

;; measures tempo part-names

(compose)

;; (partition-by #(>= % 3)
;;               [0 1 3 3 2 4 3 3 3 1 2 4 2 3 3 3 2])
