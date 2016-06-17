(ns melos.lib.melos-test
  (:require [clojure
             [data :refer [diff]]
             [test :refer [are deftest is testing use-fixtures]]
             [zip :as zip]]
            [clojure.algo.generic.functor :as functor]
            [clojure.math
             [combinatorics :as combinatorics]
             [numeric-tower :as math]]
            [melos
             [chord :as chord]
             [chord-seq :as chord-seq]
             [measure :as measure]
             [note :as note]
             [part :as part]
             [schemas :as ms]
             [utils :as utils]]
            [schema test
             [core :as s]])
  (:import [melos.schemas Chord Note]))

(use-fixtures :once schema.test/validate-schemas)

(defn select-chord-keys
  [ks form]
  (clojure.walk/postwalk
   (fn [node]
     (cond (instance? Note node)
           (select-keys node ks)
           (instance? Chord node)
           (merge (select-keys node ks)
                  {:events (into #{} (:events node))})
           :else
           node))
   form))

(def empty-diff [nil nil])

(defn diff_
  [a b]
  ((juxt first second) (diff a b)))

(deftest make-note
  (testing "throws Exception when input params are not valid"
    (is (thrown? Exception
                 (note/make-note {:pitch "0"})))
    (is (thrown? Exception
                 (note/make-note {:invalid-key nil}))))

  (testing "make-note creates a Note"
    (let [params {:count 1
                  :dissonance-contributor? true
                  :group 'group-symbol
                  :is-rest? false
                  :max-count 1234
                  :merge-left? false
                  :merge-right? true
                  :notation {:registration "A"}
                  :part :piano-voice-1
                  :pitch 10}
          note (note/make-note params)
          result (select-keys note (keys params))]
      (is (not-empty result))
      (is (= params result)))))

(defn test-chord
  []
  (let [params {:pitches [0 1 2 3]
                :dissonance-contributor? true
                :duration 9/8
                :max-count 1234
                :merge-left? false
                :merge-right? true
                :tempo 132}]
    (chord/make-chord params)))

(deftest make-chord
  (testing "make-chord creates a chord from params"
    (let [chord (test-chord)]
      (is (= (select-chord-keys [:duration :pitch :tempo] chord)
             {:duration 9/8
              :tempo 132
              :events #{{:pitch 0}
                        {:pitch 1}
                        {:pitch 2}
                        {:pitch 3}}}))))
  (testing "all notes in a chord belong to the same :group"
    (let [chord (test-chord)]
      (is (= (count (set (chord/select-chord-key :group chord)))
             1))))
  (testing "make-chord creates a chord from default params"
    (is (instance? Chord (chord/make-chord {})))))

(deftest contains-part?
  (is (= ((chord/contains-part? #{:a}) {})
         false))
  (is (= ((chord/contains-part? #{:a}) {:part :a})
         true))
  (is (= ((chord/contains-part? #{:a}) {:part :b})
         false)))

(deftest remove-part
  (is (= (chord/remove-parts* #{:a :d} [{:part :a}
                                        {:part :b}
                                        {:part :c}
                                        {:part :d}])
         [{:part :b}
          {:part :c}]))
  (is (= (chord/remove-parts* #{} [{:part :a}
                                   {:part :b}
                                   {:part :c}
                                   {:part :d}])
         [{:part :a}
          {:part :b}
          {:part :c}
          {:part :d}]))
  (is (= (chord/remove-parts* #{:a :b :c :d} [{:part :a}
                                              {:part :b}
                                              {:part :c}
                                              {:part :d}])
         [])))

(deftest remove-parts
  (let [notes [{:pitch 0 :part :a}
               {:pitch 1 :part :b}
               {:pitch 2 :part :c}
               {:pitch 3 :part :d}]
        chord (chord/make-chord {:duration 1/4
                                 :events (map note/make-note notes)})
        result (chord/remove-parts [:a :d] chord)]
    (is (= (select-chord-keys #{:pitch :part} result)
           {:events #{{:pitch 1 :part :b}
                      {:pitch 2 :part :c}}}))))

(deftest merge-chords
  (testing "merges two chords distinct by :part and :pitch"
    (let [a (chord/make-chord {:pitches [0 1]
                               :dissonance-contributor? true
                               :duration 1/4
                               :part :a
                               :tempo 60})
          b (chord/make-chord {:pitches [2 3]
                               :dissonance-contributor? true
                               :duration 4/4
                               :part :b
                               :tempo 120})
          merged (chord-seq/merge-chords a b)]
      (is (= (select-chord-keys [:duration :pitch :tempo :part]
                                merged)
             {:duration 4/4
              :tempo 120
              :events #{{:pitch 0 :part :a}
                        {:pitch 1 :part :a}
                        {:pitch 2 :part :b}
                        {:pitch 3 :part :b}}}))))

  (testing "merges two chords distinct by :part"
    (let [a (chord/make-chord {:pitches [0 1]
                               :dissonance-contributor? true
                               :duration 1/4
                               :part :a
                               :tempo 60})
          b (chord/make-chord {:pitches [0 1]
                               :dissonance-contributor? true
                               :duration 4/4
                               :part :b
                               :tempo 120})
          merged (chord-seq/merge-chords a b)]
      (is (= (select-chord-keys [:duration :pitch :tempo :part]
                                merged)
             {:duration 4/4
              :tempo 120
              :events #{{:pitch 0 :part :a}
                        {:pitch 1 :part :a}
                        {:pitch 0 :part :b}
                        {:pitch 1 :part :b}}}))))

  (testing "if a :part is present in both `a` and `b`, the result will only contain elements present in `b`"
    (let [a (chord/make-chord {:pitches [0 1]
                               :dissonance-contributor? true
                               :duration 1/4
                               :part :a
                               :tempo 60})
          b (chord/make-chord {:pitches [2 3]
                               :dissonance-contributor? true
                               :duration 4/4
                               :part :b
                               :tempo 120})
          c (chord/make-chord {:pitches [4 5]
                               :dissonance-contributor? true
                               :duration 4/4
                               :part :a
                               :tempo 120})
          merged (chord-seq/merge-chords (chord-seq/merge-chords a b)
                                         c)]
      (is (= (select-chord-keys [:duration :pitch :tempo :part]
                                merged)
             {:duration 4/4
              :tempo 120
              :events #{{:pitch 2 :part :b}
                        {:pitch 3 :part :b}
                        {:pitch 4 :part :a}
                        {:pitch 5 :part :a}}})))))

(deftest phrase
  (testing "validates phrase"
    (let [chord-data [{:pitches [0] :part :a}
                      {:pitches [1] :part :a}
                      {:pitches [2] :part :a}]
          phrase (map chord/make-chord chord-data)]
      (is (s/validate ms/Phrase phrase))))
  (testing "empty phrases are invalid"
    (is (thrown? Exception
                 (s/validate ms/Phrase [])))))

(deftest pitches->pitchclasses
  (is (= (chord/pitches->pitchclasses [0 1 2])
         (chord/pitches->pitchclasses [-12 -11 -10])
         (chord/pitches->pitchclasses [0 1 2 -12 -11 -10])
         #{0 1 2}))
  (is (= (chord/pitches->pitchclasses []) #{})))

(deftest all-intervals
  (is (= (chord/all-intervals [0 1 2])
         (chord/all-intervals [-12 -11 -10])
         (chord/all-intervals [0 1 2 -12 -11 -10])
         #{[0 1] [0 2] [1 2]}))
  (is (= (chord/all-intervals []) #{})))

(deftest interval->num
  (are [input expected] (= (chord/interval->num input) expected)
    [0 2]    2
    [0 -2]   2
    [-2 0]   2
    [-2 -14] 12
    [14 -14] 28))

(deftest dissonance-value
  (let [mapping-a {0 1
                   1 1
                   2 2
                   3 3
                   4 4
                   5 5
                   6 6}
        mapping-b {0 0,
                   1 10,
                   2 4,
                   3 3,
                   4 2,
                   5 1,
                   6 5}]
    (testing "empty pitch seq returns `0`"
      (is (= (chord/scaled-dissonance-value {} []) 0)))
    (testing "sums dissonance-value score"
      (are [a b] (= (chord/dissonance-value mapping-a a) b)
        [] 0
        [1 2 3] 6
        [1 6] 7))
    (testing "scales dissonance-value so that chords with different numbers of pitches can be compared"
      (are [a b] (= (chord/scaled-dissonance-value mapping-a a) b)
        [0 2] 2
        [0 6] 6
        [1 3] 2
        [0 1 2] 4/3
        [1 2 3] 4/3))
    (testing "m2 can be considered more dissonant than M2"
      (is (> (chord/scaled-dissonance-value mapping-b [0 1])
             (chord/scaled-dissonance-value mapping-b [0 2]))))
    (testing "M2 can be considered more dissonant than m3"
      (is (> (chord/scaled-dissonance-value mapping-b [0 2])
             (chord/scaled-dissonance-value mapping-b [0 3]))))
    (testing "P4 and P5 are equally dissonant/consonant"
      (is (= (chord/scaled-dissonance-value mapping-b [0 5])
             (chord/scaled-dissonance-value mapping-b [0 7])))
      (is (= (chord/scaled-dissonance-value mapping-b [0 5 10])
             (chord/scaled-dissonance-value mapping-b [0 7 14]))))))

(deftest consonant?
  (let [mapping {0 0,
                 1 6,
                 2 4,
                 3 3,
                 4 2,
                 5 1,
                 6 5}]
    (are [limit pitches] (= (chord/consonant? mapping limit pitches))
      [0 1] [0]
      [0 4] [0 12]
      [0 4] [0 7]
      [0 4] [0 5]
      [0 1] [0 2]
      [0 1 0 0 0] [0 2 2 2 2 2]
      [-24 -23 0 12 24] [0 2]
      [0 4 7 10] [0 4 7]
      [0 3 7 10] [0 3 7]
      [0 1] [0 6])))

(defn map->chord'
  [m]
  (clojure.walk/prewalk
   (fn [form]
     (if (and (map? form)
              (contains? form :pitches))
       (chord/make-chord form)
       form))
   m))

(deftest cycle-event-seqs
  (testing "interleaves phrases and cycles the source materials"
    (let [phrase-data {:a [[{:pitches [0] :part :a :duration 1}
                            {:pitches [1] :part :a :duration 2}
                            {:pitches [2] :part :a :duration 3 :phrase-end? true}]
                           [{:pitches [0 1] :part :a :duration 4 :phrase-end? true}]]
                       :b [[{:pitches [10 20] :part :b :duration 5 :phrase-end? true}
                            {:pitches [11 21] :part :b :duration 6 :phrase-end? true}]]}
          event-seqs (map->chord' phrase-data)
          accessors [:a :b :a :b]
          result (chord-seq/cycle-event-seqs accessors event-seqs)]
      (is (s/validate [ms/Phrase] (:a event-seqs)))
      (is (s/validate [ms/Phrase] (:b event-seqs)))
      (is (s/validate ms/Phrase result))
      (is (= empty-diff
             (diff_
              (select-chord-keys [:pitch :part :duration] result)
              [{:duration 1 :events #{{:pitch 0 :part :a}}}
               {:duration 2 :events #{{:pitch 1 :part :a}}}
               {:duration 3 :events #{{:pitch 2 :part :a}}}
               {:duration 5 :events #{{:pitch 10 :part :b} {:pitch 20 :part :b}}}
               {:duration 6 :events #{{:pitch 11 :part :b} {:pitch 21 :part :b}}}
               {:duration 4 :events #{{:pitch 0 :part :a} {:pitch 1 :part :a}}}
               {:duration 5 :events #{{:pitch 10 :part :b} {:pitch 20 :part :b}}}
               {:duration 6 :events #{{:pitch 11 :part :b} {:pitch 21 :part :b}}}]))))))

(deftest partition-phrases
  (testing "partition-groups segments phrases correctly"
    (are [expected result] (= expected result)
      [1 3 2 1 4 2]
      (->> [{:phrase-end? true}
            {:phrase-end? false}
            {:phrase-end? false}
            {:phrase-end? true}
            {:phrase-end? false}
            {:phrase-end? true}
            {:phrase-end? true}
            {:phrase-end? false}
            {:phrase-end? false}
            {:phrase-end? false}
            {:phrase-end? true}
            {:phrase-end? false}
            {:phrase-end? true}]
           (utils/partition-groups :phrase-end? [] [])
           (map count))
      [1 1 1 1]
      (->> [{:phrase-end? true}
            {:phrase-end? true}
            {:phrase-end? true}
            {:phrase-end? true}]
           (utils/partition-groups :phrase-end? [] [])
           (map count))
      [4]
      (->> [{:phrase-end? false}
            {:phrase-end? false}
            {:phrase-end? false}
            {:phrase-end? false}]
           (utils/partition-groups :phrase-end? [] [])
           (map count)))))

(deftest extend-events
  (testing "sanity check: maybe-extend works when used with `reductions`"
    (let [fn_ (chord-seq/maybe-extend (fn [a b] true) concat)]
      (is (= (fn_ [1] [2])
             [1 2]))
      (is (= (reductions fn_ [[1] [2] [3] [4]])
             [[1] [1 2] [1 2 3] [1 2 3 4]])))
    (let [fn_ (chord-seq/maybe-extend (fn [a b] false) concat)]
      (is (= (reductions fn_ [[1] [2] [3] [4]])
             [[1] [2] [3] [4]]))))
  (testing "merges two chords"
    (let [fn_ (chord-seq/maybe-extend (fn [a b] true) chord-seq/merge-chords)
          a (chord/make-chord {:pitches [0] :part :a})
          b (chord/make-chord {:pitches [1] :part :b})
          result (fn_ a b)]
      (is (= (select-chord-keys [:pitch :part] result)
             {:events #{{:pitch 0 :part :a} {:pitch 1 :part :b}}}))))
  (testing "merges chords when used with `reductions` and increments :count"
    (let [fn_ (chord-seq/maybe-extend (fn [a b] true) chord-seq/merge-chords)
          chords (map chord/make-chord [{:pitches [0] :part :a :count 0}
                                        {:pitches [1] :part :b :count 0}
                                        {:pitches [2] :part :c :count 0}
                                        {:pitches [3] :part :d :count 0}])
          result (reductions fn_ chords)]
      (is (= (select-chord-keys [:pitch :part :count 0] result)
             [{:events #{{:pitch 0 :part :a :count 0}}}
              {:events #{{:pitch 0 :part :a :count 1}
                         {:pitch 1 :part :b :count 0}}}
              {:events #{{:pitch 0 :part :a :count 2}
                         {:pitch 1 :part :b :count 1}
                         {:pitch 2 :part :c :count 0}}}
              {:events #{{:pitch 0 :part :a :count 3}
                         {:pitch 1 :part :b :count 2}
                         {:pitch 2 :part :c :count 1}
                         {:pitch 3 :part :d :count 0}}}])))
    (let [fn_ (chord-seq/maybe-extend (fn [a b] true) chord-seq/merge-chords)
          chords (map chord/make-chord [{:pitches [0] :part :a}
                                        {:pitches [1] :part :b}
                                        {:pitches [2] :part :a}
                                        {:pitches [3] :part :b}])
          result (reductions fn_ chords)]
      (is (= (select-chord-keys [:pitch :part] result)
             [{:events #{{:pitch 0 :part :a}}}
              {:events #{{:pitch 0 :part :a}
                         {:pitch 1 :part :b}}}
              {:events #{{:pitch 1 :part :b}
                         {:pitch 2 :part :a}}}
              {:events #{{:pitch 2 :part :a}
                         {:pitch 3 :part :b}}}]))))
  (testing "merge chords using `consonant?` as predicate"
    (let [mapping {0 0
                   1 6
                   2 4
                   3 3
                   4 2
                   5 1
                   6 5}
          fn_ (chord-seq/maybe-extend
               (fn [a b] (->> [a b]
                              (apply chord-seq/merge-chords)
                              (chord/select-chord-key :pitch)
                              (chord/consonant? mapping [0 4 7])))
               chord-seq/merge-chords)
          chords (map chord/make-chord [{:pitches [0] :part :a}
                                        {:pitches [7] :part :b}
                                        {:pitches [12] :part :a}
                                        {:pitches [4] :part :b}])
          result (reductions fn_ chords)]
      (is (= (select-chord-keys [:pitch :part] result)
             [{:events #{{:pitch 0 :part :a}}}
              {:events #{{:pitch 0 :part :a}
                         {:pitch 7 :part :b}}}
              {:events #{{:pitch 7 :part :b}
                         {:pitch 12 :part :a}}}
              {:events #{{:pitch 12 :part :a}
                         {:pitch 4 :part :b}}}])))))

(def default-mapping
  (functor/fmap #(math/expt % 10/9)
                {0 0,
                 1 10,
                 2 4,
                 3 3,
                 4 2,
                 5 1,
                 6 5}))

(deftest reduce-dissonance
  (testing "get candidates"
    (is (= (into #{} (map set (chord/get-candidates #{1 2 3})))
           #{#{1 2} #{1 3} #{2 3}})))
  (testing "reduce dissonance once"
    (let [chord (chord/make-chord {:events (map note/make-note [{:pitch 0 :count 2}
                                                                {:pitch 2 :count 0}
                                                                {:pitch 7 :count 1}])})]
      (is (= (select-chord-keys [:pitch :count]
                                (chord/reduce-dissonance default-mapping [0 7] chord))
             {:events #{{:pitch 2 :count 0}
                        {:pitch 7 :count 1}}}))))
  (testing "reduce dissonance"
    (let [chord (chord/make-chord {:events (map note/make-note [{:pitch 0 :count 1}
                                                                {:pitch 2 :count 0}
                                                                {:pitch 3 :count 2}])})]
      (is (= (select-chord-keys [:pitch :count]
                                (chord/reduce-dissonance default-mapping [0 2] chord))
             {:events #{{:pitch 0 :count 1}
                        {:pitch 2 :count 0}}}))))
  (testing "if below dissonance threshold, return input"
    (let [chord (chord/make-chord {:events (map note/make-note [{:pitch 0 :count 0}
                                                                {:pitch 2 :count 1}
                                                                {:pitch 3 :count 2}])})]
      (is (= (select-chord-keys [:pitch :count]
                                (chord/reduce-dissonance default-mapping [0 1] chord))
             {:events #{{:pitch 0 :count 0}
                        {:pitch 2 :count 1}
                        {:pitch 3 :count 2}}})))))

;;------------------------------------------------------------------------
;; merge-phrases
;;------------------------------------------------------------------------

(defn merge-all
  []
  (fn f
    ([xs]
     (f [] xs))
    ([a b]
     (chord-seq/merge-chords a b))))

(defn merge-phrases-1'
  [limit]
  (fn f
    ([xs]
     (f [] xs))
    ([a b]
     (let [pitches (->> (last b)
                        (chord-seq/merge-chords (last a))
                        (chord/select-chord-key :pitch))]
       (if (chord/consonant? default-mapping limit pitches)
         (map (partial chord-seq/merge-chords (last a)) b)
         b)))))

(defn merge-phrases-1
  [phrases]
  (apply concat (reductions (merge-phrases-1' [0 3])
                            (utils/partition-groups
                             :phrase-end?
                             [] []
                             phrases))))

(defn merge-phrases-2
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

(defn merge-phrases-3
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

(deftest merge-phrases-by-gradual-dissonance-reduction
  (let [phrases (map chord/make-chord [{:pitches [12] :part :a}
                                       {:pitches [14] :part :a}
                                       {:pitches [16] :part :a :phrase-end? true}
                                       {:pitches [5] :part :b}
                                       {:pitches [7] :part :b}
                                       {:pitches [8] :part :b :phrase-end? true}
                                       {:pitches [2] :part :c}
                                       {:pitches [4] :part :c}
                                       {:pitches [5] :part :c :phrase-end? true}])]

    (testing "initial state; merge-all"
      (is (= empty-diff
             (diff_
              (select-chord-keys [:pitch] (reductions (merge-all) phrases))
              [{:events #{{:pitch 12}}}
               {:events #{{:pitch 14}}}
               {:events #{{:pitch 16}}}
               {:events #{{:pitch 16} {:pitch 5}}}
               {:events #{{:pitch 16} {:pitch 7}}}
               {:events #{{:pitch 16} {:pitch 8}}}
               {:events #{{:pitch 16} {:pitch 8} {:pitch 2}}}
               {:events #{{:pitch 16} {:pitch 8} {:pitch 4}}}
               {:events #{{:pitch 16} {:pitch 8} {:pitch 5}}}]))))

    (testing "merge phrase-by-phrase; drop accumulated result if last event
in new phrase introduces a dissonance"
      (is (= (select-chord-keys [:pitch]
                                (merge-phrases-1 phrases))
             [{:events #{{:pitch 12}}}
              {:events #{{:pitch 14}}}
              {:events #{{:pitch 16}}}
              {:events #{{:pitch 16} {:pitch 5}}}
              {:events #{{:pitch 16} {:pitch 7}}}
              {:events #{{:pitch 16} {:pitch 8}}}
              {:events #{{:pitch 2}}}
              {:events #{{:pitch 4}}}
              {:events #{{:pitch 5}}}])))

    (testing "merge phrase-by-phrase; only compare final events in phrases;
merge all others. Reset if needed"
      (is (= (select-chord-keys [:pitch]
                                (reductions (merge-phrases-2 [0 3]) phrases))
             [{:events #{{:pitch 12}}}
              {:events #{{:pitch 14}}}
              {:events #{{:pitch 16}}}
              {:events #{{:pitch 16} {:pitch 5}}}
              {:events #{{:pitch 16} {:pitch 7}}}
              {:events #{{:pitch 16} {:pitch 8}}}
              {:events #{{:pitch 16} {:pitch 8} {:pitch 2}}}
              {:events #{{:pitch 16} {:pitch 8} {:pitch 4}}}
              {:events #{{:pitch 5}}}])))

  (testing "merge phrase-by-phrase; gradually reduce dissonance of final
chord in each phrase"
    (is (= (select-chord-keys [:pitch]
                              (reductions (merge-phrases-3 [0 3]) phrases))
           [{:events #{{:pitch 12}}}
            {:events #{{:pitch 14}}}
            {:events #{{:pitch 16}}}
            {:events #{{:pitch 16} {:pitch 5}}}
            {:events #{{:pitch 16} {:pitch 7}}}
            {:events #{{:pitch 16} {:pitch 8}}}
            {:events #{{:pitch 16} {:pitch 8} {:pitch 2}}}
            {:events #{{:pitch 16} {:pitch 8} {:pitch 4}}}
            {:events #{{:pitch 8} {:pitch 5}}}])))))

(deftest get-melodic-events
  (let [notes [{:pitch 0 :count 3}
               {:pitch 1 :count 0}
               {:pitch 2 :count 0}]]
    (is (= (select-chord-keys
            [:pitch :count]
            (chord/get-melodic-events
             (chord/make-chord {:events (map note/make-note notes)})))
           #{{:pitch 1 :count 0}
             {:pitch 2 :count 0}}))))

(deftest merge-adjacent?
  (are [as bs result]
      (let [a (chord/make-chord {:events (map note/make-note as)})
            b (chord/make-chord {:events (map note/make-note bs)})]
        (is (= result (chord-seq/merge-adjacent? a b))))
    [{:pitch 0 :merge-right? true :part :a}]
    [{:pitch 2 :merge-left? true :part :b}]
    true
    [{:pitch 0 :merge-right? false :part :a}]
    [{:pitch 2 :merge-left? true :part :b}]
    false
    [{:pitch 0 :merge-right? true :part :a}]
    [{:pitch 2 :merge-left? false :part :b}]
    false
    [{:pitch 0 :merge-right? true :part :a}]
    [{:pitch 2 :merge-left? true :part :a}]
    false))

(defn test-merge-horizontally
  [ks consonance-pred chords expected]
  (let [phrase
        (map (fn [notes]
               (chord/make-chord {:events (map note/make-note notes)}))
             chords)
        result (select-chord-keys
                ks
                (chord-seq/merge-horizontally consonance-pred phrase))]
    (is (= result expected))))

(deftest merge-horizontally
  (testing "merges consecutive chords into one if their sum can be considered
consonant and they all allow left- and right-merge"
    (test-merge-horizontally
     [:pitch]
     (fn [_ _] true) ;; fake consonance-pred, always true
     [[{:pitch 12 :part :a :count 0 :merge-left? false :merge-right? true}]
      [{:pitch 14 :part :b :count 0 :merge-left? true :merge-right? true}]
      [{:pitch 16 :part :c :count 0 :merge-left? true :merge-right? false}]]
     [{:events #{{:pitch 12} {:pitch 14} {:pitch 16}}}]
     ))
  (testing "does not merge if not consonant; conses instead"
    (test-merge-horizontally
     [:pitch]
     (fn [_ _] false) ;; always false
     [[{:pitch 12 :part :a :count 0 :merge-left? false :merge-right? true}]
      [{:pitch 14 :part :b :count 0 :merge-left? true :merge-right? true}]
      [{:pitch 16 :part :c :count 0 :merge-left? true :merge-right? false}]]
     [{:events #{{:pitch 12}}}
      {:events #{{:pitch 14}}}
      {:events #{{:pitch 16}}}]))
  (testing "does not merge consecutive melodic events in the same part; merges
notes with the same :pitch but different :part"
    (test-merge-horizontally
     [:pitch :part]
     (fn [_ _] true)
     [
      [{:pitch 12 :part :a :count 0 :merge-left? true :merge-right? true}]
      [{:pitch 14 :part :a :count 0 :merge-left? true :merge-right? true}]
      [{:pitch 16 :part :a :count 0 :merge-left? true :merge-right? true}]
      [{:pitch 14 :part :b :count 0 :merge-left? true :merge-right? true}]
      [{:pitch 16 :part :c :count 0 :merge-left? true :merge-right? true}]
      ]
     [{:events #{{:pitch 12 :part :a}}}
      {:events #{{:pitch 14 :part :a}}}
      {:events #{{:pitch 16 :part :a}
                 {:pitch 14 :part :b}
                 {:pitch 16 :part :c}}}])))

(deftest make-rtm-tree
  (testing "creates a valid rhythmic tree"
    (let [durations {1 [1/2 1/2]
                     1/2 [1/4 1/4]}]
      (is (s/validate ms/RhythmTreeNode
                      (measure/make-rtm-tree durations 1)))
      (is (s/validate ms/RhythmTreeNode
                      (measure/make-rtm-tree durations 1/2)))
      (is (s/validate [ms/RhythmTreeNode]
                      [(measure/make-rtm-tree durations 1)
                       (measure/make-rtm-tree durations 1)]))))
  (testing "allows multiple resolutions of one duration"
    (let [durations {1              [[3/4 :stretch] [2/4 :stretch]]
                     [3/4 :stretch] [2/4 2/4]
                     2/4            [[2/4 :stretch] 1/4]
                     [2/4 :stretch] [1/4 1/4 [1/4 :stretch]]
                     [1/4 :stretch] [1/8 1/8 1/8]
                     1/4            [1/8 1/8]}]
      (is (s/validate ms/RhythmTreeNode
                      (measure/make-rtm-tree durations 1)))
      (is (s/validate ms/RhythmTreeNode
                      (measure/make-rtm-tree durations 2/4)))
      (is (s/validate ms/RhythmTreeNode
                      (measure/make-rtm-tree durations [2/4 :stretch])))
      (is (s/validate ms/RhythmTreeNode
                      (measure/make-rtm-tree durations 1/4)))
      (is (s/validate [ms/RhythmTreeNode]
                      [(measure/make-rtm-tree durations 1)
                       (measure/make-rtm-tree durations [3/4 :stretch])
                       (measure/make-rtm-tree durations 2/4)
                       (measure/make-rtm-tree durations [2/4 :stretch])])))))

(deftest insert-into-tree
  (let [durations {1 [1/2 1/2]
                   1/2 [1/4 1/4]}]
    (testing "init tree"
      (is (= (measure/make-rtm-tree durations 1)
             {:duration 1,
              :chord nil,
              :sum-of-leaves-duration 1N,
              :type :RhythmTreeNode
              :children [{:duration 1/2,
                          :chord nil,
                          :sum-of-leaves-duration 1/2,
                          :type :RhythmTreeNode
                          :children [{:duration 1/4,
                                      :chord nil,
                                      :sum-of-leaves-duration 0,
                                      :type :RhythmTreeNode
                                      :children []}
                                     {:duration 1/4,
                                      :chord nil,
                                      :sum-of-leaves-duration 0,
                                      :type :RhythmTreeNode
                                      :children []}]}
                         {:duration 1/2,
                          :chord nil,
                          :sum-of-leaves-duration 1/2,
                          :type :RhythmTreeNode
                          :children [{:duration 1/4,
                                      :chord nil,
                                      :sum-of-leaves-duration 0,
                                      :type :RhythmTreeNode
                                      :children []}
                                     {:duration 1/4,
                                      :chord nil,
                                      :sum-of-leaves-duration 0,
                                      :type :RhythmTreeNode
                                      :children []}]}]})))

    (testing "insert chords, multiple nodes"
      (let [tree (measure/make-rtm-tree durations 1)
            zipper (measure/rtm-tree-zipper tree)
            chords (map chord/make-chord [
                                          {:pitches [0] :duration 1/4}
                                          {:pitches [1] :duration 2/4}
                                          ])
            result (measure/insert-chords chords zipper)
            ]
        (is (s/validate ms/RhythmTreeNode (zip/node zipper)))
        (is (= empty-diff
               (diff_ (select-chord-keys [:pitch :duration] result)
                      {:duration 1,
                       :chord nil,
                       :sum-of-leaves-duration 1N,
                       :type :RhythmTreeNode
                       :children [{:duration 1/2,
                                   :chord nil,
                                   :sum-of-leaves-duration 1/2,
                                   :type :RhythmTreeNode
                                   :children [{:duration 1/4,
                                               :chord {:duration 1/4 :events #{{:pitch 0}}}
                                               :sum-of-leaves-duration 0,
                                               :type :RhythmTreeNode
                                               :children []}
                                              {:duration 1/4,
                                               :chord {:duration 2/4 :events #{{:pitch 1}}}
                                               :sum-of-leaves-duration 0,
                                               :type :RhythmTreeNode
                                               :children []}]}
                                  {:duration 1/2,
                                   :chord nil,
                                   :sum-of-leaves-duration 1/2,
                                   :type :RhythmTreeNode
                                   :children [{:duration 1/4,
                                               :chord {:duration 1/4 :events #{{:pitch 1}}}
                                               :sum-of-leaves-duration 0,
                                               :type :RhythmTreeNode
                                               :children []}
                                              {:duration 1/4,
                                               :chord {:rest true}
                                               :sum-of-leaves-duration 0,
                                               :type :RhythmTreeNode
                                               :children []}]}]})))))))
