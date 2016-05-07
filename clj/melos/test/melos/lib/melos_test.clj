(ns melos.lib.melos-test
  (:require [melos.lib.note :as note]
            [melos.lib.chord :as chord]
            [melos.lib.chord-seq :as chord-seq]
            [melos.lib.schemas :as schemas]
            [melos.lib.measure :as measure]
            [melos.lib.part :as part]
            [melos.lib.utils :as utils]
            [schema.core :as s]
            [clojure.test :refer [deftest is are testing]]))

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

;; (defn get-param
;;   [kfn]
;;   (fn [node]
;;     (if (is-chord? node)
;;       (map kfn (:events node))
;;       node)))

;; (defn get-params
;;   [kfn coll]
;;   (clojure.walk/prewalk (get-param kfn) coll))

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
