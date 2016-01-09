(ns melos.score.compose-score
  (:require clojure.edn
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
             [combinations :as combinations]]
            [melos.score.materials
             [measures :as measures]
             [stepwise-mod :as stepwise-mod]]
            [progressbar.core :refer [progressbar]]
            [schema.core :as s]))

;;-----------------------------------------------------------------------------
;; SCORE

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
            (chord/scaled-dissonance-value [0 1 2]))
      7/4
      1/4)))

(defn compose-parts
  [measures tempo part-names event-seq]
  (let [head (stepwise-mod/maybe-split-vertical-moment
              (first event-seq))
        event-seq-mod (concat head (rest event-seq))
        last-event-dur (get-last-event-duration event-seq)]
    (->> event-seq
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

(defn calculate-sequences
  [{:keys [chord-seqs
           initial-state-fn
           post-process]}]
  (let [states (map #(assoc initial-state-fn :events %)
                    (make-chord-seqs chord-seqs))]

    (->> (map compose-event-seq (progressbar (into [] states)))
         (post-process))))

(defn write-session
  [output-dir session]
  (let [path (apply str output-dir "/" (:persist-to session))
        data (calculate-sequences (:params session))]
    (spit path (str data))))

(defn calc-all-sessions
  [analysis-dir sessions]
  (map (partial write-session analysis-dir) sessions))

(defn get-average-dissonance
  [phrase]
  (let [pitch-sets (map #(map :pitch %) phrase)
        durations (map chord/get-melodic-duration phrase)
        dissonances (map (fn [ps dur]
                           (* (chord/scaled-dissonance-value ps)
                              dur))
                         pitch-sets
                         durations)]
    (/ (apply + dissonances) (count pitch-sets))))

(defn read-in-data
  [analysis-dir session]
  (let [path (apply str analysis-dir "/" (:persist-to session))]
    (clojure.edn/read-string (slurp path))))

(defn compose
  [analysis-dir sessions]
  (let [segments (map (comp :segments (partial read-in-data analysis-dir)) sessions)
        indexed-segments (zipmap (range) segments)]
    (mapcat (fn [x y]
              (map (partial compose-parts
                            y
                            200
                            [:upper :lower :ped])
                   x))
            (combinations/weave-seqs indexed-segments)
            (repeat [measures/measure-4]))))

;; Phrases, start- and end-points: the end of a phrase is usually connected to the start of the next one -- intervals between phrases matter.

;; Superfluous time signatures?

;; TODO: better validity checks -- check when data is passed between modules.
;; TODO: tighter program flow in calculate-sequences -- use (comp f1 f2 f3)
