(ns score.compose-score
  (:require [clojure.edn]
            [clojure.math.combinatorics :as combinatorics]
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
            [score.group-a :as group-a]
            [score.group-b :as group-b]
            [score.group-c :as group-c]
            [score.materials
             [measures :as measures]
             [stepwise-mod :as stepwise-mod]]))

;;-----------------------------------------------------------------------------
;; SCORE

(defn make-overlaps [pitches]
  (->> pitches
       (partition 2 1)
       (mapcat (fn [[x y]]
                 [[x] [x y]]))))

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
         ;; hardcoded
         (rhythm-tree/extend-last 2/4)
         (rhythm-tree/make-r-tree measures)
         (part/compose-part tempo part-names))))

(defn make-chord-seq
  [{:keys [upper lower ped melodic-indices]}]
  ;; hardcoded
  (let [melody-sources-atom (atom {:lower lower
                                   :upper upper
                                   :ped ped})]
    (chord-seq/collect-events-in-segment melodic-indices
                                         melody-sources-atom)))

(defn make-chord-seqs
  [source]
  (->> source
       (combinations/unfold-parameters)
       (map make-chord-seq)))

(defn parts-in-chord
  [chord]
  (set (map :part chord)))

;; hardcoded
(defn part-count-sufficient?
  [minimum chord]
  (let [part-count ((comp count parts-in-chord) chord)]
    (>= part-count minimum)))

;; hardcoded
(def partition-events-fn
  (partial part-count-sufficient? 3))

;; hardcoded
(defn filter-events-fn
  [events]
  (and (>= (count events) 6)
       (every? (partial part-count-sufficient? 3) events)))

(defn distinct-by
  "Returns a lazy sequence of the elements of coll, removing any elements that
  return duplicate values when passed to a function f."
  [f coll]
  (let [step (fn step [xs seen]
               (lazy-seq
                ((fn [[x :as xs] seen]
                   (when-let [s (seq xs)]
                     (let [fx (f x)]
                       (if (contains? seen fx)
                         (recur (rest s) seen)
                         (cons x (step (rest s) (conj seen fx)))))))
                 xs seen)))]
    (step coll #{})))

(defn pitch-profile
  [vertical-moments]
  (map (fn [x] (map :pitch x)) vertical-moments))

(defn max-pitch
  [vertical-moments]
  (->> vertical-moments
       (pitch-profile)
       (flatten)
       (apply max)))

(require '[progressbar.core :refer [progressbar]])

(defn calculate-sequences
  [{:keys [filter-fn
           distinct-by-fn
           chord-seqs
           initial-state-fn
           sort-by-fn]}]
  (let [states (map initial-state-fn (make-chord-seqs chord-seqs))]

  (->>
       (map compose-event-seq (progressbar (into [] states)))

       ((fn [x]
          (do (println "\nNumber of generated phrases:" (count x))
              x)))

       (mapcat filter-fn)

       ((fn [x]
          (do (println "Number of items before uniquify:" (count x))
              x)))

       (distinct-by distinct-by-fn)

       ((fn [x]
          (do (println "Number of items after uniquify:" (count x))
              x)))

       (drop 100)
       (take 20)
       (sort-by sort-by-fn)

       )))

(def sessions
  {
   "testing" group-a/materials
   "testing-b" group-b/materials
   "testing-c" group-c/materials
   })

(def diss-params
  {:check (fn [events]
            (<= (chord/scaled-dissonance-value (map :pitch events))
                ;; hardcoded
                (chord/scaled-dissonance-value [0 1 2])))})

(defn initial-state
  [events]
  {:events events
   :diss-fn-params {:max-count 100
                    :max-lingering 300
                    ;; TODO: pass on diss-value
                    :diss-params diss-params}
   :tempo 200
   :measures [measures/measure-4]
   :pre []
   :post []
   :part-names [:upper :lower :ped]})

(def session-config
  {:persist-to "/Users/fred/projects/music/compositions/2015/organ/analysis/testing-c.edn"
   :params {:filter-fn (fn [x]
                         (->> x
                              (partition-by partition-events-fn)
                              (filter filter-events-fn)
                              (take 1)))
            :distinct-by-fn pitch-profile
            :chord-seqs group-c/materials
            :initial-state-fn initial-state
            :sort-by-fn (fn [x] 1) }
   })

(defn new-session
  [{:keys [params persist-to]}]
  (spit
   persist-to
   (str (calculate-sequences params))))

(defn calc-all-sessions
  []
  (new-session session-config))

;; (defn calc-all-sessions
;;   []
;;   (map (fn [[k v]]
;;          (new-session k v))
;;        sessions))

(defn compose []
  (let
      [data
        [
        ;; hardcoded
        (clojure.edn/read-string
         (slurp
          (str "/Users/fred/projects/music/compositions/2015/organ/analysis/" "testing-c" ".edn")))
        ;; (clojure.edn/read-string
        ;;  (slurp
        ;;   (str "/Users/fred/projects/music/compositions/2015/organ/analysis/" "testing-b" ".edn")))
        ]
       ]

    (mapcat (fn [x y]
              (map (partial compose-parts
                            y
                            ;; hardcoded
                            200
                            ;; hardcoded
                            [:upper :lower :ped])
                   x))
            data
            ;; How combine materials from different sessions?
            ;; Interleave?
            ;; [(shuffle (apply concat data))]
            [[measures/measure-2]
             [measures/measure-2]])

    ))

;; Phrases, start- and end-points: the end of a phrase is usually connected to the start of the next one -- intervals between phrases matter.

;; Superfluous time signatures?

;; TODO: better validity checks
;; TODO: tighter program flow in calculate-sequences -- use (comp f1 f2 f3)
