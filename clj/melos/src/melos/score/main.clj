(ns melos.score.main
  (:require [cheshire.core :refer :all]
            [cheshire.generate :refer [add-encoder
                                       remove-encoder
                                       encode-map]]
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
  {1 [1/2 1/2]
   1/2 [1/4 1/4 1/4]
   1/4 [1/8 1/8]
   1/8 [1/16 1/16]})

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
                          (melos :voice-3 0 8 2)
                          (melos :voice-5 0 5 1)]))

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
       (let [pitches (->> (chord-seq/merge-chords a b)
                          (chord/select-chord-key :pitch))]
         (if (chord/consonant? default-mapping limit pitches)
           (chord-seq/merge-chords a b)
           b))
       (chord-seq/merge-chords a b)))))

(def extended-events (reductions (handle-dissonance-fn [0 2 4 5]) event-seqs))

(def part-names [:voice-1 :voice-3 :voice-5])

(defn make-rtm-tree
  [event-seq]
  (-> (measure/insert-chords event-seq rtm-tree-zipper)
      :children))

(defn make-part
  [part-name]
  {part-name
   (->> (part/filter-chords part-name extended-events)
        chord-seq/simplify-event-seq
        make-rtm-tree)})

;; TODO: sections with different instrumentation?
;; TODO: automatic decoding
(defn render
  []
  (utils/export-to-json
   "/Users/fred/projects/music/compositions/2015/organ/output/section_1.json"
   {:title "test"
    :author "anonymous"
    :score-template "asdf"
    :parse-fn "qwer"
    :sections [{:parts part-names
                :voices (apply merge (map make-part part-names))}]}))
