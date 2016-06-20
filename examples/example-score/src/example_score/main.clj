(ns example-score.main
  (:require [clojure.java.shell :as shell]
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

(defn make-phrase
  [part transposition pitches]
  (let [phrase-end-values (concat (repeat (dec (count pitches)) false) [true])
        phrase-id (gensym "phrase_id__")]
    (map (fn [pitch phrase-end?]
           (chord/make-chord {:events [(note/make-note {:pitch (+ pitch transposition)
                                                        :part part
                                                        :notation {:type :Slur
                                                                   :phrase-id
                                                                   phrase-id}})]
                              :duration 1/4
                              :phrase-end? phrase-end?}))
         pitches
         phrase-end-values)))

(defn melos
  [part transposition pitches]
  {part (map (partial make-phrase part transposition) pitches)})

(def voices (apply merge [(melos :voice-1 0 [[0] [1 2] [3 4] [5]])
                          (melos :voice-3 -3 [[0] [1 2] [3 4] [5]])
                          (melos :voice-5 -8 [[0] [1 2] [3 4] [5]])]))

;; TODO: phrase -- set type?
(def event-seqs
  (chord-seq/cycle-event-seqs (take 16 (cycle [:voice-1 :voice-3 :voice-5])) voices))

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

(def voice->staff
  {:voice-1 :a
   :voice-3 :b
   :voice-5 :c})

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
                          :voices [(make-voice :voice-3)]}
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
