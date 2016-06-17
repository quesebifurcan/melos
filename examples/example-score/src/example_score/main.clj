(ns example-score.main
  (:require [clojure.java.shell :as shell]
            [melos.lib
             [note      :as note]
             [chord     :as chord]
             [chord-seq :as chord-seq]
             [measure   :as measure]
             [part      :as part]
             [schemas   :as ms]
             [utils     :as utils]])
  (:import [melos.lib.schemas Note Chord]))

(def durations
  {1              [3/4 [1/2 :stretch]]
   3/4            [[1/2 :stretch] 1/4]
   [1/2 :stretch] [1/2 1/4]
   1/2            [1/4 1/4]
   1/4            [1/8 1/8]
   1/8            [1/16 1/16]})

(def measure-1
  (measure/make-rtm-tree durations 1))

(def rtm-tree-zipper
  (measure/rtm-tree-zipper {:root true :children [measure-1 measure-1]}))

(defn melos
  [part transposition range_ partition_]
  {part (utils/partition-groups
         :phrase-end?
         (map (fn [pitch]
                (let [phrase-end (zero? (rem pitch partition_))]
                  (chord/make-chord {:pitches [(+ pitch transposition)]
                                     :part part
                                     :duration 1/4
                                     :phrase-end? phrase-end})))
              (range 0 range_)))})

(def voices (apply merge [(melos :voice-1 0 11 3)
                          (melos :voice-3 -2 8 2)
                          (melos :voice-5 -10 5 1)]))

(def event-seqs
  (chord-seq/cycle-event-seqs (take 10 (cycle [:voice-1 :voice-3 :voice-5])) voices))

(def default-mapping
  {0 0,
   1 10,
   2 4,
   3 3,
   4 2,
   5 1,
   6 5})

(defn handle-dissonance-fn
  [limit]
  (fn f
    ([xs]
     (f [] xs))
    ([a b]
     (if (:phrase-end? b)
       (chord/reduce-dissonance default-mapping
                                limit
                                (chord-seq/merge-chords a b))
       (chord-seq/merge-chords a b)))))

(def extended-events (reductions (handle-dissonance-fn [0 2 4 5]) event-seqs))

(def part-names [:voice-1 :voice-3 :voice-5])

(defn make-rtm-tree
  [event-seq]
  (-> (measure/insert-chords event-seq rtm-tree-zipper)
      :children))

(def voice->staff
  {:voice-1 :a
   :voice-3 :b
   :voice-5 :c})

(defn make-voice
  [part-name]
  {:type :Voice
   :name part-name
   :measures (->> (part/filter-chords part-name extended-events)
                  chord-seq/simplify-event-seq
                  make-rtm-tree)})

;; TODO: sections with different instrumentation?
;; TODO: automatic decoding
;; TODO: only output selected keys?
;; TODO: add :Staff in order to be able to add staff-level markup for each section?
;; TODO: collect list of all annotations
(defn render
  []
  (utils/export-to-json
   "output/example_1.json"
   {:type :Score
    :title "test"
    :author "anonymous"
    :score-template "asdf"
    :parse-fn "qwer"
    :sections [
               {:type :Section
                :voices (map make-voice part-names)}
               {:type :Section
                :voices (map make-voice part-names)}
               ]})
  (shell/sh "scripts/to_pdf.sh"))
