(ns melos.lib.melos-test
  (:require [melos.lib.note :as note]
            [melos.lib.chord :as chord]
            [melos.lib.chord-seq :as chord-seq]
            [melos.lib.schemas :as schemas]
            [melos.lib.measure :as measure]
            [melos.lib.part :as part]
            [melos.lib.utils :as utils]
            [schema.core :as s]
            [schema.test]
            [clojure.test :refer [deftest is are testing use-fixtures]])
  (:import [melos.lib.schemas Note Chord]))

(use-fixtures :once schema.test/validate-schemas)

;; (defn get-param
;;   [kfn]
;;   (fn [node]
;;     (if (
;;       (map kfn (:events node))
;;       node)))

;; (defn get-params
;;   [kfn coll]
;;   (clojure.walk/prewalk (get-param kfn) coll))

(deftest make-note
  (testing "throws Exception when input params are not valid"
    (is (thrown? Exception
                 (schemas/make-note {:pitch "0"})))
    (is (thrown? Exception
                 (schemas/make-note {:invalid-key nil}))))
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
          note (schemas/make-note params)
          result (select-keys note (keys params))]
      (is (not-empty result))
      (is (= params result)))))

(def measure:4-4
  (measure/parse-rtm-tree-node
   (measure/stretch-tree [4 4] 0 [[0] [0] [0] [0]])))

(s/validate schemas/RhythmTreeNode
            {:written-duration [1 2]
             :duration [2 3]
             :event nil
             :children [{:written-duration [2 3]
                         :duration [1 2]
                         :event (schemas/make-note {})
                         :children nil}]})

;; measure:4-4

;; (def asdf
;;   (s/conditional #(contains? % :event)
;;                  (s/schema-with-name {:event s/Int} "a")
;;                  :else
;;                  (s/schema-with-name {:asdf s/Int} "b")))

;; (s/validate asdf {:asdf 8})

;; (deftest make-chord
;;   (testing "make-chord creates a chord"
;;     (let [params {:pitches [0 1 2 3]
;;                   :dissonance-contributor? true
;;                   :group 'group-symbol-chord
;;                   :max-count 1234
;;                   :merge-left? false
;;                   :merge-right? true
                  ;; :tempo 132


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
