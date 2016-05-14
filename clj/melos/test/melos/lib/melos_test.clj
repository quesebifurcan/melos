(ns melos.lib.melos-test
  (:require [melos.lib.note :as note]
            [melos.lib.chord :as chord]
            [melos.lib.chord-seq :as chord-seq]
            [melos.lib.schemas :as ms]
            [melos.lib.measure :as measure]
            [melos.lib.part :as part]
            [melos.lib.utils :as utils]
            [schema.core :as s]
            [schema.test]
            [clojure.test :refer [deftest is are testing use-fixtures]]
            [clojure.data :refer [diff]])
  (:import [melos.lib.schemas Note Chord]))

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
                            {:pitches [2] :part :a :duration 3}]
                           [{:pitches [0 1] :part :a :duration 4}]]
                       :b [[{:pitches [10 20] :part :b :duration 5}
                            {:pitches [11 21] :part :b :duration 6}]]}
          event-seqs (map->chord' phrase-data)
          accessors [:a :b :a :b]
          result (chord-seq/cycle-event-seqs accessors event-seqs)]
      (is (s/validate [ms/Phrase] (:a event-seqs)))
      (is (s/validate [ms/Phrase] (:b event-seqs)))
      (is (s/validate [ms/Phrase] result))
      (is (= (select-chord-keys [:pitch :part :duration] result)
             [[{:duration 1 :events #{{:pitch 0 :part :a}}}
               {:duration 2 :events #{{:pitch 1 :part :a}}}
               {:duration 3 :events #{{:pitch 2 :part :a}}}]
              [{:duration 5 :events #{{:pitch 10 :part :b} {:pitch 20 :part :b}}}
               {:duration 6 :events #{{:pitch 11 :part :b} {:pitch 21 :part :b}}}]
              [{:duration 4 :events #{{:pitch 0 :part :a} {:pitch 1 :part :a}}}]
              [{:duration 5 :events #{{:pitch 10 :part :b} {:pitch 20 :part :b}}}
               {:duration 6 :events #{{:pitch 11 :part :b} {:pitch 21 :part :b}}}]])))))

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
  (testing "merges chords when used with `reductions`"
    (let [fn_ (chord-seq/maybe-extend (fn [a b] true) chord-seq/merge-chords)
          chords (map chord/make-chord [{:pitches [0] :part :a}
                                        {:pitches [1] :part :b}
                                        {:pitches [2] :part :c}
                                        {:pitches [3] :part :d}])
          result (reductions fn_ chords)]
      (is (= (select-chord-keys [:pitch :part] result)
             [{:events #{{:pitch 0 :part :a}}}
              {:events #{{:pitch 0 :part :a}
                         {:pitch 1 :part :b}}}
              {:events #{{:pitch 0 :part :a}
                         {:pitch 1 :part :b}
                         {:pitch 2 :part :c}}}
              {:events #{{:pitch 0 :part :a}
                         {:pitch 1 :part :b}
                         {:pitch 2 :part :c}
                         {:pitch 3 :part :d}}}])))
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

(defn partition-phrases
  [xs]
  (filter identity
          (utils/partition-by-inclusive (complement :phrase-end) xs)))

(def default-mapping
  {0 0, 1 6, 2 4, 3 4, 4 2, 5 1, 6 5})

(defn merge-ph
  [a b]
  (if (chord/consonant? default-mapping [0 4 7]
                        (chord/select-chord-key :pitch
                                                (chord-seq/merge-chords (last a)
                                                                        (last b))))
    (map (fn [x] (chord-seq/merge-chords (last a) x)) b)
    b))

(deftest merge-phrases
  (let [a (map chord/make-chord [{:pitches [0] :part :b}
                                 {:pitches [2] :part :b :phrase-end true}])
        b (map chord/make-chord [{:pitches [0] :part :a}
                                 {:pitches [2] :part :a :phrase-end true}])
        c (map chord/make-chord [{:pitches [12] :part :c}
                                 {:pitches [14] :part :c :phrase-end true}])
        old (chord/make-chord {:pitches [7] :part :a})
        result (reductions merge-ph [old] (cycle [a b c]))]
    (testing "merges phrases"
      (is (= [nil nil]
             ((juxt first second)
              (diff (select-chord-keys [:pitch :part] (take 4 result))
                    [[{:events #{{:pitch 7 :part :a}}}]
                     [{:events #{{:pitch 7 :part :a}
                                 {:pitch 0 :part :b}}}
                      {:events #{{:pitch 7 :part :a}
                                 {:pitch 2 :part :b}}}]
                     [{:events #{{:pitch 2 :part :b}
                                 {:pitch 0 :part :a}}}
                      {:events #{{:pitch 2 :part :b}
                                 {:pitch 2 :part :a}}}]
                     [{:events #{{:pitch 2 :part :b}
                                 {:pitch 2 :part :a}
                                 {:pitch 12 :part :c}}}
                      {:events #{{:pitch 2 :part :b}
                                 {:pitch 2 :part :a}
                                 {:pitch 14 :part :c}}}]])))))
    (testing "is lazy"
      (is (= (count (utils/take-realized result)) 4)))))

;; (deftest segment-chords)
;; (deftest join-events)
;; (deftest extend-phrases)
;; (deftest merge-horizontally)
;; (deftest chord-seq->rhythm-tree)
;; (deftest simplify-tree)
