(ns example-score.main
  (:require [clojure.java.shell :as shell]
            [clojure.algo.generic.functor :as functor]
            [example-score.chromatic-line :refer [chromatic-line]]
            [example-score.staccato :refer [staccato]]
            [example-score.arpeggio :refer [arpeggio]]
            [example-score.pulse :refer [pulse]]
            [example-score.multi :refer [multi]]
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

(defn voices
  []
  (let [event-seqs (apply merge [

                                 {:f (staccato {:phrases phrases
                                                :part-name :voice-1
                                                :transposition -2
                                                :note-durations [1/8 1/8 1/4]
                                                :durations [1/4 1/4]})}

                                 ;; {:f (arpeggio {:phrases [[[0] [0 2] [0 2 4]]
                                 ;;                          [[0 2 4] [0 2 4 6]]]
                                 ;;                :part-name :voice-1
                                 ;;                :transposition -2
                                 ;;                :durations [1/4 1/4]})}

                                 ;; {:f (pulse {:phrases [[[0]]
                                 ;;                       [[2]]]
                                 ;;             :part-name :voice-1
                                 ;;             :transposition -2
                                 ;;             :durations [1/4 1/4]})}

                                 {:b1 (chromatic-line {:phrases [[[0] [1]]
                                                                 [[2] [3]]
                                                                 [[4] [5]]
                                                                 [[6] [7]]]
                                                       :part-name :voice-3
                                                       :transposition 5
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

                                 {:d (multi {:voices [:voice-5 :voice-3 :voice-1]
                                             :duration [1/4 2/4]
                                             :pitches [[[-20 10 0]
                                                        [-19 11 1]]
                                                       [[-18 12 2]]]})}])]
    (->> event-seqs (functor/fmap cycle) atom)))

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

(defn cycle-measures
  [dur measures]
  (if (> dur 0)
    (cons (first measures)
          (cycle-measures (- dur (:sum-of-leaves-duration (first measures)))
                          (utils/rotate measures)))))

(defn make-rtm-tree
  [measures event-seq]
  (-> (measure/insert-chords event-seq (rtm-tree-zipper measures)) :children))

(defn make-voice
  [{:keys [events part-name final-event-min-dur measure-list]}]
  (let [event-seq (part/filter-chords part-name events)
        total-duration (+ (apply + (map :duration event-seq)) final-event-min-dur)
        measures (cycle-measures total-duration measure-list)
        measures-duration (apply + (map :sum-of-leaves-duration measures))
        diff_ (- measures-duration total-duration)
        extended (concat (butlast event-seq)
                         [(update (last event-seq) :duration (fn [x] (+ x diff_ final-event-min-dur)))])]
    {:type :Voice
     :name part-name
     :measures (->> (chord-seq/simplify-event-seq extended)
                    (make-rtm-tree measures))}))

(def template-1
  {:type :Section
   :staves [{:type :Staff
             :name :a
             :notation :soft
             :voices [:voice-1]}
            {:type :Staff
             :name :b
             :notation :shrill
             :voices [:voice-3 :voice-4]}
            {:type :Staff
             :name :c
             :notation :very-soft
             :voices [:voice-5]}]})

(defn compose-voices
  [template f]
  (clojure.walk/postwalk
   (fn [form]
     (if (and (vector? form) (= (first form) :voices))
       [:voices (map f (second form))]
       form))
   template))

(defn compose-section
  [{:keys [event-seqs
           voice-seq
           dissonance-limit
           final-event-min-dur
           template
           measure-list
           merge-horizontally-fn]}]
  (let [events (->> (chord-seq/cycle-event-seqs voice-seq event-seqs)
                    (reductions (handle-dissonance-fn dissonance-limit))
                    (chord-seq/merge-horizontally merge-horizontally-fn))]
    (compose-voices template #(make-voice {:events events
                                           :part-name %
                                           :measure-list measure-list
                                           :final-event-min-dur final-event-min-dur}))))

;; TODO: sections with different instrumentation?
;; TODO: automatic decoding
;; TODO: only output selected keys?
;; TODO: collect list of all annotations
(defn render
  []
  (let [event-seqs (voices)]
    (utils/export-to-json
     "output/example_1.json"
     {:type :Score
      :title "test"
      :author "anonymous"
      :score-template "asdf"
      :parse-fn "qwer"
      :sections (mapv compose-section [{:voice-seq (take 40 (cycle [:f :b1 :c :b2 :c]))
                                        :dissonance-limit [0 2 4 5]
                                        :final-event-min-dur 7/4
                                        :template template-1
                                        :event-seqs event-seqs
                                        :measure-list [measure-1]
                                        :merge-horizontally-fn (fn [_ _] true)}])}))
  (shell/sh "scripts/to_pdf.sh"))
