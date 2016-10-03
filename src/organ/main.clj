(ns organ.main
  (:require [clojure.java.shell :as shell]
            [clojure.algo.generic.functor :as functor]
            [melos
             [chord :as chord]
             [chord-seq :as chord-seq]
             [measure :as measure]
             [note :as note]
             [part :as part]
             [schemas :as ms]
             [utils :as utils]])
  (:import [melos.schemas Chord Note]))

(defn slope [a b]
  (fn [groups]
    (mapcat (fn [count_] (utils/apply-slope count_ a b)) groups)))

(defn compose-phrases
  [{:keys [parts phrases group-level transposition] :as m}]
  (let [params (merge (select-keys m (keys (chord/chord-default)))
                      (select-keys m (keys (note/note-default)))
                      {:phrase-end?
                       ((slope false true)
                        (map count (cycle phrases)))})
        params' (atom (utils/unfold-parameters params))]
    (let [phrases-group (gensym "G__")]
      (map (fn [phrase]
             (let [phrase-group (gensym "G__")]
               (map (fn [chords]
                      (let [chord-group (gensym "G__")
                            curr-params (first @params')]
                        (do (swap! params' #(drop 1 %))
                        (reduce
                         chord-seq/merge-chords
                         (map (fn [part chord]
                                (let [g (group-level {:phrases phrases-group
                                                      :phrase phrase-group
                                                      :chord chord-group})]
                                  (chord/make-chord
                                   (merge {:part part
                                           :group g
                                           :is-rest? (= chord :rest)
                                           :pitches
                                           (if (= chord :rest)
                                             [0]
                                             (map (partial + transposition)
                                                  chord))}
                                          curr-params
                                          ))))
                              parts
                              chords)))))
                    phrase)))
           (cycle phrases)))))

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

(defn compose-section
  [{:keys [event-seqs
           voice-seq
           dissonance-limit
           final-event-min-dur
           handle-dissonance-fn
           template
           tempo
           measure-list
           merge-horizontally-fn]}]
  (let [events (->> (chord-seq/cycle-event-seqs voice-seq event-seqs)
                    handle-dissonance-fn
                    (chord-seq/merge-horizontally merge-horizontally-fn))]
    (compose-voices (template tempo) #(make-voice {:events events
                                                   :part-name %
                                                   :measure-list measure-list
                                                   :final-event-min-dur final-event-min-dur}))))

(defn get-ordering
  [m]
  (let [lcd (apply * (vals m))
        make-ranges (fn [[k v]]
                      (map (fn [x] [k x])
                           (range 0 lcd (/ lcd v))))
        reverse-sort-ks (fn [x]
                          ((zipmap (reverse (keys (sort m))) (range))
                           (first x)))
        dedupe' (fn [xs]
                  (let [result (dedupe xs)]
                    (if (= (first result) (last result))
                      (butlast result)
                      result)))]
    (->> m
         (mapcat make-ranges)
         (sort-by (juxt second reverse-sort-ks))
         (map first)
         dedupe')))

;;------------------------------------------------------------------------------

(defn phrases
  [n]
  (take n [[[[-3]]]
           [[[-3 -1]]]
           [[[-1]]]
           [[[0]]]
           [[[1]] [[2]] [[2 4]]]
           [[[4]] [[4 5]]]
           [[[5]]]
           [[[5]] [[5 -3]]]]))

(defn phrases-2
  [n]
  [
   [[[-3]]]
   [[[-3 -1]]]
   [[[-3 -1 0]]]
   [[[-1 0]]]
   [[[0]]]
   [[[0 2]]]
   [[[0 2 4]]]
   [[[2 4]]]
   [[[4]]]
   [[[4 5]]]
   [[[4 5 7]]]
   [[[5 7]]]
   [[[7]]]
   [[[7 9]]]
   [[[7 9 -3]]]
   [[[9 -3]]]
   ])

(defn phrases-3
  [drop-n take-n]
  (take take-n (drop drop-n
                     [[[[-3]]]
                      [[[-2]] [[-1]]]
                      [[[0]]]
                      [[[1]] [[2]]]
                      [[[3]] [[4]]]
                      [[[5]]]
                      [[[6]] [[7]]]
                      [[[8]] [[9]]]])))

(defn phrases-4
  [n]
  (take n [
           [[[-3]] [[-2]]]
           [[[-1]] [[0]]]
           [[[1]] [[2]]]
           [[[3]]]
           [[[4]] [[5]]]
           [[[6]] [[7]]]
           [[[8]]]
           ]))

(defn durs
  [ph]
  (let [slopes (map (fn [end] (slope 1/4 end))
                    [2/4 3/4 4/4 6/4])]
    (mapcat (fn [s cnt]
              (s [cnt]))
            slopes
            (map count ph))))

;; TODO: highest pitch?
(defn voices
  []
  (let [event-seqs (apply merge [

                                 {:a3 (compose-phrases
                                      {:parts [:voice-1]
                                       :duration [1/4]
                                       :transposition 0
                                       :notation [{:type :arpeggio}]
                                       :group-level :phrases
                                       :check-dissonance [true]
                                       :phrases (phrases-4 7)})}

                                 {:c3 (compose-phrases
                                      {:parts [:voice-3]
                                       :duration [1/4]
                                       :transposition 0
                                       :notation [{:type :arpeggio}]
                                       :group-level :phrases
                                       :check-dissonance [true]
                                       :phrases (phrases-4 6)})}

                                 {:e3 (compose-phrases
                                      {:parts [:voice-5]
                                       :duration [1/4]
                                       :transposition -12
                                       ;; :notation [{:type :pulse
                                       ;;             :subdivisions 5
                                       ;;             :pattern [0 0 1]}]
                                       :notation [{:type :arpeggio}]
                                       :group-level :phrases
                                       :check-dissonance [true]
                                       :phrases (phrases-4 5)})}

                                 {:a (compose-phrases
                                      {:parts [:voice-1]
                                       :duration [1/4]
                                       :transposition 0
                                       :notation [{:type :arpeggio}]
                                       :group-level :phrases
                                       :check-dissonance [true]
                                       :phrases (phrases-3 0 8)})}

                                 {:b (compose-phrases
                                      {:phrases
                                       [[[[]] [[-3]]]
                                        [[[]] [[-3 -1]]]
                                        [[[]] [[-3 -1 0]]]
                                        [[[]] [[-1 0]]]
                                        [[[]] [[0]]]
                                        [[[]] [[-3 0]]]]
                                       :parts [:voice-2]
                                       :notation [{:type :arpeggio}]
                                       :check-dissonance [true]
                                       :group-level :phrases
                                       :transposition 12
                                       :duration [1/4]})}

                                 ;; {:c (compose-phrases
                                 ;;      {:phrases [[[[0]]]
                                 ;;                 [[[1]]]
                                 ;;                 [[[2]]]
                                 ;;                 [[[3]]]
                                 ;;                 [[[4]]]
                                 ;;                 [[[5]]]
                                 ;;                 [[[6]]]
                                 ;;                 [[[7]]]
                                 ;;                 [[[8]]]]
                                 ;;       :parts [:voice-3]
                                 ;;       :transposition -3
                                 ;;       :notation [{:type :arpeggio}]
                                 ;;       :group-level :phrases
                                 ;;       :check-dissonance [false]
                                 ;;       :duration [1/4]})}

                                 {:c (compose-phrases
                                      {:parts [:voice-3]
                                       :duration [1/4]
                                       :transposition 0
                                       :notation [{:type :arpeggio}]
                                       :group-level :phrases
                                       :check-dissonance [true]
                                       :phrases (phrases-3 1 6)})}

                                 {:c2 (compose-phrases
                                       {:parts [:voice-3]
                                        :duration [1/4]
                                        :transposition 0
                                        :notation [{:type :arpeggio}]
                                        :group-level :phrase
                                        :check-dissonance [false]
                                        :phrases [
                                                  [[[-1]] [[-1 11]]]
                                                  [[[11]] [[11 9]] [[11 9 -1]]]
                                                  [[[9]] [[-3 9]] [[-3 9 -1]]]
                                                  ]})}

                                 {:d (compose-phrases
                                      {:parts [:voice-4]
                                       :duration [1/4]
                                       :transposition 0
                                       :notation [{:type :arpeggio}]
                                       :group-level :phrases
                                       :check-dissonance [false]
                                       :phrases (phrases 4)})}

                                 {:e (compose-phrases
                                      {:parts [:voice-5]
                                       :duration [1/4]
                                       :transposition -12
                                       ;; :notation [{:type :pulse
                                       ;;             :subdivisions 5
                                       ;;             :pattern [0 0 1]}]
                                       :notation [{:type :arpeggio}]
                                       :group-level :phrases
                                       :check-dissonance [true]
                                       :phrases (phrases-3 2 4)})}

                                 {:g (compose-phrases
                                      {:parts [:voice-5 :voice-3 :voice-1]
                                       :duration [2/4 1/4 2/4 3/4 4/4]
                                       :group-level :phrase
                                       :transposition 0
                                       :check-dissonance [true]
                                       :phrases [[[[] [] []]
                                                  [:rest :rest :rest]
                                                  [[-19] [] []]
                                                  [[] [-7] []]
                                                  [[] [] [17]]]]})}

                                 {:h (compose-phrases
                                      {:parts [:voice-5]
                                       :duration [2/4
                                                  3/4
                                                  4/4
                                                  5/4
                                                  6/4]
                                       :transposition 0
                                       :group-level :chord
                                       :check-dissonance [false]
                                       :phrases [[[[-9 -7]]]
                                                 [[[-10 -6]]]
                                                 [[[-12 -6]]]
                                                 [[[-14 -6]]]]})}

                                 {:i (compose-phrases
                                      {:parts [:voice-3]
                                       :duration [2/4
                                                  3/4
                                                  4/4
                                                  5/4]
                                       :transposition 0
                                       :group-level :chord
                                       :check-dissonance [false]
                                       :phrases [[[[2]]]
                                                 [[[3]]]
                                                 [[[4]]]
                                                 [[[5]]]
                                                 [[[6]]]]})}

                                 {:j (compose-phrases
                                      {:parts [:voice-1]
                                       :duration [2/4
                                                  3/4
                                                  4/4]
                                       :transposition 0
                                       :group-level :chord
                                       :check-dissonance [false]
                                       :phrases [[[[12]]]
                                                 [[[11]]]
                                                 [[[12]]]
                                                 [[[12]]]
                                                 [[[11]]]
                                                 [[[11]]]]})}
                                 {:k (compose-phrases
                                      {:parts [:voice-1]
                                       :duration [1/4]
                                       :transposition 3
                                       :notation [{:type :arpeggio}]
                                       :group-level :phrases
                                       :check-dissonance [false]
                                       :phrases (phrases-2 4)})}

                                 ])]
    (->> event-seqs
         (functor/fmap cycle)
         atom)))

(def default-mapping
  {0 0,
   1 21,
   2 5,
   3 3,
   4 2,
   5 1,
   6 8})

;; (defn filter-out-dissonant
;;   [xs]
;;   (let [part-count (fn [chord] (count (set (chord/select-chord-key :part chord))))
;;         groups (partition-by (fn [x] (= (part-count x) 1)) xs)
;;         one-counts (mapv (fn [x] (= (part-count (first x)) 1)) groups)]
;;     (->> (map (fn [chord group one-count? next-one-count?]
;;                 (cond one-count?
;;                       [(last group)]
;;                       (and next-one-count?
;;                            (:phrase-end? chord))
;;                       (concat (butlast group)
;;                               [(assoc (last group) :duration 3/4)])
;;                       :else
;;                       group
;;                       ))
;;               (cycle (rest xs))
;;               groups
;;               one-counts
;;               (cycle (rest one-counts)))
;;          (apply concat))))

(defn apply-dissonance-filter?
  [chord]
  (and (:phrase-end? chord)
       (:check-dissonance chord)))

(defn handle-dissonance-fn'
  [limit]
  (fn f
    ([xs]
     (f [] xs))
    ([a b]
     (if (apply-dissonance-filter? b)
       (chord/reduce-dissonance default-mapping
                                limit
                                (chord-seq/merge-chords a b))
       (chord-seq/merge-chords a b)))))

(defn sustain-if-short
  [xs]
  (map (fn [a b]
         (let [prev-mel (filter #(zero? (:count %)) (:events a))
               prev-mel (map (juxt :part :pitch) prev-mel)
               new-mel (map (juxt :part :pitch) (:events b))]
           (if (and (:phrase-end? a)
                    (not (some (set prev-mel) (set new-mel))))
             (assoc a :duration 3/4)
             a)))
       xs
       (cycle (rest xs))))

(defn handle-dissonance-fn
  [dissonance-limit]
  (fn [events]
    (->> (reductions (handle-dissonance-fn' dissonance-limit) events)
         sustain-if-short)))

(defn merge-horizontally-fn
  [mapping limit]
  (fn [a b]
    (->> [a b]
         chord-seq/merge-chords
         (chord/select-chord-key :pitch)
         (chord/consonant? mapping limit))))

(defn template-1
  [tempo]
  {:type :Section
   :staves [{:type :Staff
             :tempo tempo
             :name :a
             :voices [:voice-1]}
            {:type :Staff
             :name :b
             :voices [:voice-2]}
            {:type :Staff
             :name :c
             :voices [:voice-3]}
            {:type :Staff
             :name :d
             :voices [:voice-4]}
            {:type :Staff
             :name :e
             :voices [:voice-5]}]})

(defn sections
  []
  (let [event-seqs (voices)]
    (utils/rotate-values-sequentially
     {:voice-seq [(take 30 (cycle (get-ordering {:a 5 :c 4 :b 4 :d 2 :e 1})))]
      :handle-dissonance-fn [(handle-dissonance-fn [0 1])
                             (handle-dissonance-fn [0 2 3])
                             (handle-dissonance-fn [0 2 4 5])]
      :final-event-min-dur [5/4]
      :tempo [178]
      :template [template-1]
      :event-seqs [event-seqs]
      :measure-list [[measure-2] [measure-1]]
      :merge-horizontally-fn [(fn [_ _] false)]}
     [[:handle-dissonance-fn]
      [:handle-dissonance-fn]
      [:handle-dissonance-fn]])))

;; TODO: sections with different instrumentation?
;; TODO: only output selected keys?
(defn render
  []
  (let [filename "output/example_1.json"]
    (utils/export-to-json
     filename
     {:type :Score
      :title "test"
      :author "anonymous"
      :sections (mapv compose-section (sections))})
    (println "Abjad...")
    (shell/sh "scripts/show.sh" filename)))
