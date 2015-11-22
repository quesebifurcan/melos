(ns melos.score.compose-score
  (:require clojure.edn
            [clojure.math.combinatorics :as combinatorics]
            [progressbar.core :refer [progressbar]]
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
             [combinations :as combinations]
             [group-a :as group-a]
             [group-b :as group-b]
             [group-c :as group-c]]
            [melos.score.materials
             [measures :as measures]
             [stepwise-mod :as stepwise-mod]]
            [schema.core :as s]))

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

(defn get-last-event-duration
  [events]
  (let [lst (last events)]
    (if (>= (chord/scaled-dissonance-value (map :pitch lst))
            (chord/scaled-dissonance-value [0 2 4 5]))
      7/4
      1/4)))

(defn compose-parts
  [measures tempo part-names event-seq]
  (let [head (stepwise-mod/maybe-split-vertical-moment
              (first event-seq))
        event-seq-mod (concat head (rest event-seq))
        last-event-dur (get-last-event-duration event-seq-mod)]
    (->> event-seq-mod
         ;; hardcoded
         (rhythm-tree/extend-last last-event-dur)
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

;; (defn max-pitch
;;   [vertical-moments]
;;   (->> vertical-moments
;;        (pitch-profile)
;;        (flatten)
;;        (apply max)))

(defn calculate-sequences
  [{:keys [filter-fn
           distinct-by-fn
           chord-seqs
           initial-state-fn
           sort-by-fn]}]
  (let [states (map #(assoc initial-state-fn :events %) 
                    (make-chord-seqs chord-seqs))]

  (->>
       (map compose-event-seq (progressbar (into [] states)))

       ((fn [x]
          (do (println "\nNumber of generated phrases:" (count x))
              x)))

       (mapcat filter-fn)

       ((fn [x]
          (do (println "Number of items before uniquify:" (count x))
              x)))

       (utils/distinct-by distinct-by-fn)

       ((fn [x]
          (do (println "Number of items after uniquify:" (count x))
              x)))

       (drop 100)
       (take 20)
       (sort-by sort-by-fn))))

(def sessions
  {
   "testing" group-a/materials
   "testing-b" group-b/materials
   "testing-c" group-c/materials
   })

(defn new-session
  [{:keys [params persist-to]}]
  (spit
   persist-to
   (str (calculate-sequences params))))

(defn calc-all-sessions
  []
  (new-session group-c/session-config))

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
    (doall (map (fn [x]
                  (doall (map (fn [y] (println (count y))) x))) data))

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
            [[measures/measure-3]
             [measures/measure-3]])

    ))

;; Phrases, start- and end-points: the end of a phrase is usually connected to the start of the next one -- intervals between phrases matter.

;; Superfluous time signatures?

;; TODO: better validity checks
;; TODO: tighter program flow in calculate-sequences -- use (comp f1 f2 f3)
