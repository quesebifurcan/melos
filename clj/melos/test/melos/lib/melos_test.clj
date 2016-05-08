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
            [clojure.test :refer [deftest is are testing use-fixtures]])
  (:import [melos.lib.schemas Note Chord]))

(use-fixtures :once schema.test/validate-schemas)

(defn select-chord-key [k chord] (map k (:events chord)))

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
      (is (= (count (set (select-chord-key :group chord)))
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

;;   ;;   (let [a (chord/make-chord {:pitches [0 2 7]
;;   ;;                                :dissonance-contributor? true
;;   ;;                                :duration 1/4
;;   ;;                                :part :a
;;   ;;                                :tempo 60})
;;   ;;         b (chord/make-chord {:pitches [0]
;;   ;;                                :dissonance-contributor? true
;;   ;;                                :duration 4/4
;;   ;;                                :part :a
;;   ;;                                :tempo 120})
;;   ;;         merged (chord-seq/merge-chords a b)]
;;   ;;     (is (= (select-chord-keys [:duration :pitch :tempo :part]
;;   ;;                               merged)
;;   ;;            {:duration 4/4
;;   ;;             :tempo 120
;;   ;;             :events #{{:pitch 0 :part :a}
;;   ;;                       {:pitch 2 :part :a}
;;   ;;                       {:pitch 7 :part :a}}}))
;;   ;;     (is (= (ms/select-chord-key :group b)
;;   ;;            (ms/select-chord-key :group merged))))
;;   ;;   (let [a (chord/make-chord {:pitches [0 2]
;;   ;;                                :dissonance-contributor? true
;;   ;;                                :duration 1/4
;;   ;;                                :part :a
;;   ;;                                :tempo 60})
;;   ;;         b (chord/make-chord {:pitches [0 2 7]
;;   ;;                                :dissonance-contributor? true
;;   ;;                                :duration 4/4
;;   ;;                                :part :a
;;   ;;                                :tempo 120})
;;   ;;         merged (chord-seq/merge-chords a b)]
;;   ;;     (is (= (select-chord-keys [:duration :pitch :tempo :part]
;;   ;;                               merged)
;;   ;;            {:duration 4/4
;;   ;;             :tempo 120
;;   ;;             :events #{{:pitch 0 :part :a}
;;   ;;                       {:pitch 7 :part :a}
;;   ;;                       {:pitch 2 :part :a}}})))))
;;   )

;; TODO:
;; rules for merging

;; (deftest round-robin)
;; (deftest dissonance-values)
;; (deftest consonant?)
;; (deftest segment-chords)
;; (deftest join-events)
;; (deftest extend-phrases)
;; (deftest merge-horizontally)
;; (deftest chord-seq->rhythm-tree)
;; (deftest simplify-tree)

;; (def measure:4-4
;;   (measure/parse-rtm-tree-node
;;    (measure/stretch-tree [4 4] 0 [[0] [0] [0] [0]])))

;; (s/validate schemas/RhythmTreeNode
;;             {:written-duration [1 2]
;;              :duration [2 3]
;;              :event nil
;;              :children [{:written-duration [2 3]
;;                          :duration [1 2]
;;                          :event (schemas/make-note {})
;;                          :children nil}]})

;; (defn make-chord [{:keys [duration pitches part] :as m}]
;;   (let [group (gensym "G__")
;;         make-note' (fn [pitch]
;;                      (note/make-note {:part part :pitch pitch :group group}))]
;;     {:duration duration
;;      :events (map make-note' pitches)}))

;; (defn is-chord?
;;   [node]
;;   (and (map? node)
;;        (contains? node :events)))

;; (deftest make-note-test
;;   (testing "Initializes note with default parameters"
;;     (is (s/validate schemas/Note (note/make-note {:part :test}))))
;;   (testing "Creates unique groups"
;;     (let [notes [(note/make-note {:part :test})
;;                  (note/make-note {:part :test})
;;                  (note/make-note {:part :test})]
;;           groups (map :group notes)]
;;       (is (= (count notes)
;;              (count (set groups)))))))

;; ;; concatenates chords
;; ;; adds note to chord
;; ;; extract params from chord
;; ;; updates chord
;; (deftest make-chord-test
;;   (let [chords [{:duration 1/4
;;                  :pitches [1 2 3 4]
;;                  :part :upper}
;;                 {:duration 1/4
;;                  :pitches [1 2 2 3 4 5 6]
;;                  :part :upper}
;;                 {:duration 1/4
;;                  :pitches [10 20]
;;                  :part :upper}]]
;;     (doseq [input chords]
;;       (testing "make-chord generates valid chords"
;;         (is (s/validate schemas/Chord (make-chord input))))
;;       (testing "creates one note for each pitch"
;;         (is (= (count (:pitches input))
;;                (count (:events (make-chord input))))))
;;       (testing "all notes have the same :group"
;;         (let [group-count (->> (make-chord input)
;;                                (get-params :group)
;;                                set
;;                                count)]
;;           (is (= 1 group-count))))
;;       (testing "all notes have the same :part"
;;         (let [expected #{(:part input)}
;;               result (->> (make-chord input)
;;                           (get-params :part)
;;                           set)]
;;           (is (= expected result)))))))

;; (defrecord Note_ [pitch duration])
;; (defrecord Chord_ [duration events])

;; (s/validate {:pitch s/Int :duration s/Num}
;;             (map->Note_ {:pitch 1 :duration 1/4}))

;; (map->Chord_ {:duration 1 :events [(map->Note_ {})]})

;; (let [a (map->Note_ {:duration (map map->Note_ [{:duration 1} {:duration 2} {:duration 3}])})]
;;   (clojure.walk/prewalk
;;    (fn [node]
;;      (if (instance? Note_ node)
;;        (assoc node :pitch 129384719283749)
;;        node))
;;    a))
