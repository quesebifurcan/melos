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

(defn segment-range
  [filter_ r]
  (let [[start end] r
        r' (range start (inc end))]
    (->> r'
         (utils/partition-by-inclusive
          (fn [x] (not (some (fn [x'] (= x' (rem (+ 60 x) 12))) filter_))))
         (mapv #(filter identity %))
         (map (fn [x] (reverse (take 1 (reverse x)))))
         (mapv (fn [x] (map (fn [y] [y]) x)))
         ;; (#(conj % [[23]]))
         )))

(def measure-2
  (let [durations {1 [2/4 2/4]
                   2/4 [1/4 1/4]
                   1/4 [1/8 1/8]}]
    (measure/make-rtm-tree durations 1)))

(def measure-1
  (let [durations {1              [3/4 [1/2 :stretch]]
                   3/4            [[1/2 :stretch] 1/4]
                   [1/2 :stretch] [1/2 1/4]
                   1/2            [1/4 1/4]
                   1/4            [1/8 1/8]
                   1/8            [1/16 1/16]}]
    (measure/make-rtm-tree durations 1)))

(defn rtm-tree-zipper
  [measures]
  (measure/rtm-tree-zipper {:root true :children measures}))

(def phrases [[[0 2 4] [1] [2]]
              [[3] [4]]
              [[5] [6] [7]]
              [[8] [9]]])

(def phrases [
              [[0]]
              [[1]]
              [[2]]
              [[1] [0]]
              [[1] [2]]
              [[1]]
              [[0]]
              [[1] [2]]
              [[1] [0]]
              [[1] [2]]
              [[1]]
              [[0]]
              ;; [[1] [2]]
              ;; [[3]]
              ;; [[4]]
              ;; [[5]]
              ;; [[4]]
              ;; [[3]]
              ;; [[2]]
              ;; [[1]]
              ;; [[0]]
              [[1] [2]]
              [[1]]
              [[0]]
              [[1]]
              [[0]]
              [[1] [2]]
              [[1]]
              ])

;; (def phrases [
;;               [[0]]
;;               [[7]]
;;               [[0]]
;;               [[7]]
;;               [[12]]
;;               [[0]]
;;               [[7]]
;;               [[12]]
;;               [[7]]
;;               [[0]]
;;               [[12]]
;;               [[0]]
;;               [[12]]
;;               [[0]]
;;               [[12]]
;;               [[0]]
;;               [[12]]
;;               ])

(def phrase
  [[[0] [2]]
   [[7]]
   [[12]]
   [[2]]
   [[0]]
   [[2] [3]]
   [[5]]
   [[3] [5]]
   [[7]]
   [[0]]
   [[2]]])

(def phrase
  [
   [[0] [1]]
   [[2]]
   [[3]]
   [[0]]
   [[1]]
   [[2] [3]]
   [[0]]
   [[1]]
   [[2]]
   [[3]]
   [[0]]
   [[1] [2]]
   [[3]]

   [[0] [1]]
   [[2]]
   [[3]]
   [[0]]
   [[1]]
   [[2] [3]]
   [[0]]
   [[1]]
   [[2]]
   [[3]]
   [[0]]
   [[1] [2]]
   [[3]]

   [[0] [1]]
   [[2]]
   [[3]]
   [[0]]
   [[1]]
   [[2] [3]]
   [[0]]
   [[1]]
   [[2]]
   [[3]]
   [[0]]
   [[1] [2]]
   [[3]]

   [[0]]
   [[1]]
   [[2]]
   [[2]]
   [[2]]
   [[2]]
   [[2]]
   [[2]]
   [[2]]
   [[2]]
   [[2]]
   [[2]]
   [[2]]
   [[2]]
   [[2]]
   [[2]]
   [[2]]
   [[2]]

   ])

(defn phrase-a
  [r]
  (let [result (segment-range [0 2 4 0 2 4 5 0 2 4 5 7 0 2 4 5 7 9] r)]
    result))
  ;; (utils/transpose-all (first r) phrase))

(def components
  {:melodies {:a [1 2 3]
              :b [4 5 6]}
   :sequences {:x [:a :b]
               :y [:a :a :b]}
   :measures {:a [measure-1]
              :b [measure-2 measure-1]}})

(defn phrases
  [n]
  (take n [[[-3]]
           [[-2] [-1]]
           [[0]]
           [[1] [2]]
           [[3] [4]]
           [[5]]
           [[6] [7]]
           [[8] [9]]
           ]
        ))

;; (defn phrases
;;   [n]
;;   [
;;    [[-3]]
;;    [[-2] [-1] [0]]
;;    [[1] [2]]
;;    [[-3]]
;;    [[-2] [-1] [0]]
;;    [[1] [2]]
;;    [[3] [4]]
;;    [[-3]]
;;    [[-2] [-1] [0]]
;;    [[1] [2]]
;;    [[3] [4]]
;;    [[5]]
;;    [[6] [7]]
;;    [[-3]]
;;    [[-2] [-1] [0]]
;;    [[1] [2]]
;;    [[3] [4]]
;;    [[5]]
;;    [[6] [7]]
;;    [[8] [9]]]
;;   )

;; TODO: highest pitch?
(defn voices
  []
  (let [event-seqs (apply merge [
                                 {:a (chromatic-line
                                      {:phrases (phrases 7)
                                       :part-name :voice-1
                                       :transposition 0
                                       :check-dissonance [false]
                                       :durations [1/4]})}

                                 ;; {:b (chromatic-line
                                 ;;      {:phrases (phrases 6)
                                 ;;       :part-name :voice-2
                                 ;;       :transposition 0
                                 ;;       :durations [1/4]})}

                                 {:b (arpeggio
                                      {:phrases [
                                                 [[-3]]
                                                 [[-3 -1]]
                                                 [[-3 -1 0]]
                                                 [[-1 0]]
                                                 [[0]]
                                                 [[-3 0]]
                                                 ]
                                       :part-name :voice-2
                                       :transposition 12
                                       :durations [1/4]})}

                                 ;; {:b (multi {:voices [:voice-2 :voice-3 :voice-4]
                                 ;;             :duration [2/4 2/4]
                                 ;;             :phrase-end? [false true true true]
                                 ;;             :pitches [[[0 2 4]
                                 ;;                        [2 4 5]]
                                 ;;                       [[4 5 7]]
                                 ;;                       [[5 7 9]]
                                 ;;                       ]})}

                                 ;;                 [[-1 11]]
                                 ;;                 [[11]]
                                 ;;                 [[11 9]]
                                 ;;                 [[9]]
                                 ;;                 [[9 -3]]
                                 ;;                 [[-3]]
                                 ;;                 [[-3 -1]]]
                                 ;;       :part-name :voice-3
                                 ;;       :transposition 0
                                 ;;       :durations [1/4]})}

                                 {:c (chromatic-line
                                      {:phrases [
                                                 [[0]]
                                                 [[1]]
                                                 [[2]]
                                                 [[3]]
                                                 [[4]]
                                                 [[5]]
                                                 [[6]]
                                                 [[7]]
                                                 [[8]]
                                                 ]
                                       :part-name :voice-3
                                       :transposition -3
                                       :check-dissonance [false]
                                       :durations [1/4]})}

                                 {:d (chromatic-line
                                      {:phrases (phrases 4)
                                       :part-name :voice-4
                                       :transposition 0
                                       :check-dissonance [false]
                                       :durations [1/4]})}

                                 {:e (chromatic-line
                                      {:phrases (phrases 3)
                                       :part-name :voice-5
                                       :transposition -12
                                       :check-dissonance [false]
                                       :durations [1/4]})}

                                 ;; {:f (staccato {:phrases phrases
                                 ;;                :part-name :voice-1
                                 ;;                :transposition -2
                                 ;;                :note-durations [1/8 1/8 1/4]
                                 ;;                :durations [1/4 1/4]})}

                                 ;; {:a (arpeggio {:phrases [[[0] [0 2] [0 2 4]]
                                 ;;                          [[0 2 4] [0 2 4 6]]]
                                 ;;                :part-name :voice-1
                                 ;;                :transposition -2
                                 ;;                :durations [1/4 1/4]})}

                                 ;; ;; {:f (pulse {:phrases [[[0]]
                                 ;; ;;                       [[2]]]
                                 ;; ;;             :part-name :voice-1
                                 ;; ;;             :transposition 22
                                 ;; ;;             :durations [1/4 1/4]})}

                                 ;; {:f (chromatic-line {:phrases [[[0] [1]]
                                 ;;                                [[2] [3]]
                                 ;;                                [[4]]
                                 ;;                                [[0] [1]]
                                 ;;                                [[2]]
                                 ;;                                [[3] [4]]
                                 ;;                                [[5]]]
                                 ;;                      :part-name :voice-1
                                 ;;                      :transposition 1
                                 ;;                      :durations [1/4 1/4]})}

                                 ;; {:g (chromatic-line {:phrases [[[2]]]
                                 ;;                      :part-name :voice-2
                                 ;;                      :transposition 0
                                 ;;                      :durations [1/4]})}

                                 ;; {:b1 (chromatic-line {:phrases [[[7]]]
                                 ;;                       :part-name :voice-3
                                 ;;                       :transposition 0
                                 ;;                       :durations [1/4]})}

                                 ;; {:b2 (chromatic-line {:phrases [[[-2]]
                                 ;;                                 [[0]]
                                 ;;                                 [[2]]]
                                 ;;                       :part-name :voice-4
                                 ;;                       :transposition -3
                                 ;;                       :durations [1/4]})}

                                 ;; {:c (arpeggio {:phrases [[[0]]
                                 ;;                          [[1]]
                                 ;;                          [[2]]
                                 ;;                          [[3]]
                                 ;;                          [[4]]]
                                 ;;                :part-name :voice-5
                                 ;;                :transposition -20
                                 ;;                :durations [1/4]})}

                                 {:f (multi {:voices [:voice-5 :voice-3 :voice-1]
                                             :duration [1/4 2/4]
                                             :pitches [[[-20 10 0]
                                                        [-19 11 1]]
                                                       [[-18 12 2]]]})}])]
    (->> event-seqs (functor/fmap cycle) atom)))

(def default-mapping
  {0 0,
   1 21,
   2 5,
   3 3,
   4 2,
   5 1,
   6 8})

(defn handle-dissonance-fn
  [limit]
  (fn f
    ([xs]
     (f [] xs))
    ([a b]
     (if (and (:phrase-end? b)
              (:check-dissonance b))
       (let [result (chord/reduce-dissonance default-mapping
                                             limit
                                             (chord-seq/merge-chords a b))]
         (if (and (= (chord/select-chord-key :pitch result)
                     (chord/select-chord-key :pitch b))
                  (not= (count (chord/select-chord-key :pitch a))
                        1))
           (assoc result :dissonance-drop true)
           result))
       (chord-seq/merge-chords a b)))))

(defn merge-horizontally-fn
  [limit]
  (fn [a b]
    (chord/consonant? default-mapping
                      limit
                      (chord/select-chord-key :pitch
                                              (chord-seq/merge-chords a b)))))

(defn cycle-measures
  [dur measures]
  (if (> dur 0)
    (let [head (first measures)]
      (cons head
            (cycle-measures (- dur (:sum-of-leaves-duration head))
                            (utils/rotate measures))))))

(defn make-rtm-tree
  [measures event-seq]
  (-> (measure/insert-chords event-seq (rtm-tree-zipper measures)) :children))

(defn sum-by-key [k xs] (apply + (map k xs)))

(defn extend-last
  [duration xs]
  (concat (butlast xs)
          [(update (last xs) :duration #(+ % duration))]))

(defn make-voice
  [{:keys [events part-name final-event-min-dur measure-list]}]
  (let [event-seq         (part/filter-chords part-name events)
        total-duration    (+ (sum-by-key :duration event-seq)
                             final-event-min-dur)
        measures          (cycle-measures total-duration measure-list)
        measures-duration (sum-by-key :sum-of-leaves-duration measures)
        overhang          (- measures-duration total-duration)
        extended          (extend-last (+ final-event-min-dur overhang)
                                       event-seq)]
    {:type :Voice
     :name part-name
     :measures (->> (chord-seq/simplify-event-seq extended)
                    (make-rtm-tree measures))}))

(defn template-1
  [tempo]
  {:type :Section
   :staves [
            {:type :Staff
             :tempo tempo
             :name :a
             ;; :notation :soft
             :voices [:voice-1]}
            {:type :Staff
             :name :b
             ;; :notation :soft
             :voices [:voice-2]}
            {:type :Staff
             :name :c
             ;; :notation :soft
             :voices [:voice-3]}
            {:type :Staff
             :name :d
             ;; :notation :shrill
             :voices [:voice-4]}
            {:type :Staff
             :name :e
             ;; :notation :very-soft
             :voices [:voice-5]}]})

(defn template-2
  [tempo]
  {:type :Section
   :staves [
            {:type :Staff
             :tempo tempo
             :name :a
             ;; :notation :soft
             :voices [:voice-1]}
            {:type :Staff
             :name :c
             ;; :notation :soft
             :voices [:voice-3]}
            {:type :Staff
             :name :e
             ;; :notation :very-soft
             :voices [:voice-5]}]})


(defn voices-entry?
  [form]
  (and (vector? form)
       (= (first form) :voices)))

(defn compose-voices' [f form] [:voices (map f (second form))])

(defn compose-voices
  [template f]
  (clojure.walk/postwalk
   (fn [form]
     (if (voices-entry? form)
       (compose-voices' f form)
       form))
   template))

(defn filter-out-dissonant
  [xs]
  (let [part-count (fn [chord] (count (set (chord/select-chord-key :part chord))))
        groups (partition-by (fn [x] (= (part-count x) 1)) xs)
        one-counts (mapv (fn [x] (= (part-count (first x)) 1)) groups)]
    (->> (map (fn [group one-count? next-one-count?]
                (cond one-count?
                      [(last group)]
                      next-one-count?
                      (concat (butlast group)
                              [(assoc (last group) :duration 2/4)])
                      :else
                      group
                  ))
              groups
              one-counts
              (cycle (rest one-counts)))
         (apply concat))))

(defn compose-section
  [{:keys [event-seqs
           voice-seq
           dissonance-limit
           final-event-min-dur
           template
           tempo
           measure-list
           merge-horizontally-fn]}]
  (let [events (->> (chord-seq/cycle-event-seqs voice-seq event-seqs)
                    (reductions (handle-dissonance-fn dissonance-limit))
                    (filter-out-dissonant)
                    ;; ((fn [z]
                    ;;    (map (fn [x y] (if (:dissonance-drop y)
                    ;;                     (assoc x :duration 7/4)
                    ;;                     x))
                    ;;         z
                    ;;         (cycle (rest z)))))
                    ;; ((fn [z]
                    ;;    (map (fn [x y]
                    ;;           (if (chord/consonant? default-mapping
                    ;;                                 [0 1]
                    ;;                                 (chord/select-chord-key
                    ;;                                  :pitch y))
                    ;;             x
                    ;;             (assoc x :duration 3/4)))
                    ;;         z
                    ;;         (cycle (rest z)))))
                    (map (fn [x]
                           (update x :events
                                   (fn [events]
                                     (map (fn [y]
                                            (update y :pitch #(+ % 0)))
                                          events)))))
                    (chord-seq/merge-horizontally merge-horizontally-fn))]
    (compose-voices (template tempo) #(make-voice {:events events
                                                   :part-name %
                                                   :measure-list measure-list
                                                   :final-event-min-dur final-event-min-dur}))))

;; Can every "problematic" set of voices be "resolved" through a particular _sequence_ of voice events?

(defn p1 [a b c d e] [e c b d a c a c d a c b d c b e b d])

(defn sections
  []
  (let [event-seqs (voices)]
    (utils/rotate-values-sequentially
     {
      :voice-seq [
                  (take 60 (cycle [:e :d :c :b :a]))
                  ;; (take 30 (cycle [:a :c :e]))
                  ]
      :dissonance-limit [[0 2 3]]
      :final-event-min-dur [5/4]
      :tempo [163]
      :template [template-1]
      :event-seqs [event-seqs]
      :measure-list [[measure-2] [measure-1]]
      ;; :merge-horizontally-fn [(fn [_ _] true)]}
      :merge-horizontally-fn [(merge-horizontally-fn [0 2 4 5])]}
     ;; "link" two or more params. can be useful for :template/:voice-seq
     [
      [:voice-seq]
      [:voice-seq]
      [:voice-seq]
      ])))

;; TODO: sections with different instrumentation?
;; TODO: only output selected keys?
(defn render
  []
  (utils/export-to-json
   "output/example_1.json"
   {:type :Score
    :title "test"
    :author "anonymous"
    :score-template "asdf"
    :parse-fn "qwer"
    :sections (mapv compose-section (sections))})
  (println "Abjad...")
  (shell/sh "scripts/to_pdf.sh"))
