(ns example-score.main
  (:require [clojure.java.shell :as shell]
            [example-score.chromatic-line :refer [chromatic-line]]
            [example-score.staccato :refer [staccato]]
            [example-score.arpeggio :refer [arpeggio]]
            [example-score.pulse :refer [pulse]]
            [melos
             [chord :as chord]
             [chord-seq :as chord-seq]
             [measure :as measure]
             [note :as note]
             [part :as part]
             [schemas :as ms]
             [utils :as utils]])
  (:import [melos.schemas Chord Note]))

(def durations
  {1              [3/4 [1/2 :stretch]]
   3/4            [[1/2 :stretch] 1/4]
   [1/2 :stretch] [1/2 1/4]
   1/2            [1/4 1/4]
   1/4            [1/8 1/8]
   1/8            [1/16 1/16]})

(def measure-1
  (measure/make-rtm-tree durations 1))

(defn rtm-tree-zipper
  [measures]
  (measure/rtm-tree-zipper {:root true :children measures}))

(def phrases [[[0] [1] [2]]
              [[3] [4]]
              [[5] [6] [7]]
              [[8] [9]]])

(def voices
  (apply merge [

                ;; {:a (staccato {:phrases phrases
                ;;                :part-name :voice-1
                ;;                :transposition -2
                ;;                :note-durations [1/8 1/8 1/4]
                ;;                :durations [1/4 1/4]})}

                ;; {:a (arpeggio {:phrases [[[0] [0 2] [0 2 4]]
                ;;                          [[0 2 4] [0 2 4 6]]]
                ;;                :part-name :voice-1
                ;;                :transposition -2
                ;;                :durations [1/4 1/4]})}

                {:a (pulse {:phrases [[[0]]
                                      [[2]]]
                            :part-name :voice-1
                            :transposition -2
                            :durations [1/4 1/4]})}

                {:b1 (chromatic-line {:phrases [[[0] [1]]
                                                [[2] [3]]
                                                [[4] [5]]
                                                [[6] [7]]]
                                     :part-name :voice-3
                                     :transposition 12
                                     :durations [1/4]})}
                {:b2 (chromatic-line {:phrases [[[-1]]
                                                [[-2]]
                                                [[-1]]
                                                [[-2]]]
                                     :part-name :voice-4
                                     :transposition 0
                                     :durations [1/4]})}
                {:c (chromatic-line {:phrases phrases
                                     :part-name :voice-5
                                     :transposition -20
                                     :durations [1/4]})}
                ]))

(def event-seqs
  (chord-seq/cycle-event-seqs (take 16 (cycle [:a :b1 :b2 :c])) voices))

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

(def extended-events
  (->> (reductions (handle-dissonance-fn [0 2 4 5]) event-seqs)
       (chord-seq/merge-horizontally (fn [_ _] true))))

(def part-names [:voice-1 :voice-3 :voice-5])

(defn cycle-measures
  [dur measures]
  (if (> dur 0)
    (cons (first measures)
          (cycle-measures (- dur (:sum-of-leaves-duration (first measures)))
                          (utils/rotate measures)))))

(defn make-rtm-tree
  [measures event-seq]
  (-> (measure/insert-chords event-seq (rtm-tree-zipper measures))
      :children))

(defn make-voice
  [part-name]
  (let [event-seq (part/filter-chords part-name extended-events)
        total-duration (apply + (map :duration event-seq))
        measures (cycle-measures total-duration [measure-1])
        measures-duration (apply + (map :sum-of-leaves-duration measures))
        diff_ (- measures-duration total-duration)
        extended (concat (butlast event-seq)
                         [(update (last event-seq) :duration (fn [x] (+ x diff_)))])]
    {:type :Voice
     :name part-name
     :measures (->> (chord-seq/simplify-event-seq extended)
                    (make-rtm-tree measures))}))

;; TODO: sections with different instrumentation?
;; TODO: automatic decoding
;; TODO: only output selected keys?
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
                :staves [{:type :Staff
                          :name :a
                          :notation :soft
                          :voices [(make-voice :voice-1)]}
                         {:type :Staff
                          :name :b
                          :notation :shrill
                          :voices [(make-voice :voice-3)
                                   (make-voice :voice-4)]}
                         {:type :Staff
                          :name :c
                          :notation :very-soft
                          :voices [(make-voice :voice-5)]}]}
               {:type :Section
                :staves [
                         {:type :Staff
                          :name :a
                          :notation :asdf
                          :voices [(make-voice :voice-1)]}
                         {:type :Staff
                          :name :b
                          :notation :asdf
                          :voices [(make-voice :voice-3)]}
                         {:type :Staff
                          :name :c
                          :notation :asdf
                          :voices [(make-voice :voice-5)]}
                         ]}
               ]
    })
  (shell/sh "scripts/to_pdf.sh"))

;; sections -- tempo, measures, registration etc.
;; pluggable gestures?
;; tempi
;; avoid repeated time signatures
;; reproducability / versioning?
