(ns melos.lib.chord-seq-test
  (:require [melos.lib.note :as note]
            [melos.lib.chord :as chord]
            [melos.lib.chord-seq :as chord-seq]
            [melos.lib.schemas :as ms]
            [melos.lib.utils :as utils]
            [clojure.test :as test]
            [clojure.test.check :as tc]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [schema.core :as s]
            [schema-generators.complete :as c]
            [schema-generators.generators :as g]))

(def Note*
  (sorted-map :pitch (s/enum 0 2 4 5 7 9)
              :duration (s/enum 1/16 1/8 2/1)
              :is-rest? s/Bool
              :part (s/enum :lower :upper :ped)
              :max-count s/Int))

;; ;; (map note/make-note (g/sample 10 Note*))

;; (gen/sample (gen/elements [:lower :upper]))

;; list of notes

;; (defspec note-list
;;   (prop/for-all [x (gen/fmap note/make-note
;;                              (g/generator Note*))]
;;     (test/is (s/validate s/Int (:pitch x)))))

(def phrase-data
  {:part (s/enum :a :b :c)})

;; (defn make-phrase
;;   [part-name]
;;   (gen/fmap note/make-note

;; (def chord-template
;;   {:cnt (s/enum 1 2 3)
;;    :part (s/enum :lower :upper)})

;; (gen/sample
;;  (gen/fmap (fn [{:keys [cnt part]}]
;;              (gen/sample (g/generate Note*)
;;            (g/generator chord-template))
;;  2)
;;
;; (defn make-note
;;   [params]
;;   (note/make-note (merge (g/generate Note*) params)))

;; (def chords
;;   (gen/fmap (fn [[cnt part]]
;;               ;; (repeatedly cnt (partial note/make-note {:part part})))
;;               (repeatedly cnt (partial make-note {:part part})))
;;             (gen/tuple (gen/elements [1 2])
;;                        (gen/elements [:upper :lower]))))

;; ;; (map (fn [x]
;; ;;        (map :part x))
;; ;;      (gen/sample len-var))

;; (defn make-chord
;;   [[pitches duration part]]
;;   (utils/make-chord-from-pitch-vector-params {:pitch pitches
;;                                               :duration duration
;;                                               :part part}))

;; (def gen-pitches
;;   (gen/set (gen/elements [0 2 4 6 7 8 9])
;;            {:min-elements 1
;;             :max-elements 3}))

;; (defn gen-chord
;;   [part]
;;   (gen/fmap make-chord
;;             (gen/tuple gen-pitches
;;                        (gen/elements [1/16 1/4 1/8 1/1])
;;                        (gen/elements [part]))))

;; (def gen-phrase
;;   (gen/fmap gen-chord
;;             (gen/elements [:asdf :qwer])))

;; (gen/sample (gen-chord :oij))

;; (g/generate [{:a s/Int}])

;; (s/validate {:a (s/pred (complement empty?))} {:a []})

;; (def oijoij
;;   {:a (s/conditional (fn [x] (not (empty? x)))
;;                      [s/Int])})

;; (s/validate
;;  oijoij
;;  {:a [8]})

;; (defn ne [body]
;;   (s/conditional (fn [x] (not (empty? x)))
;;                  body))

;; (defn count-ne [body]
;;   (s/conditional (fn [x] (and (> (count x) 0)
;;                               (< (count x) 2)))
;;                  body))

;; (g/generate (count-ne [s/Int]))

;; (defn asdf
;;   []
;;   {:a (gen/generate (gen/fmap (fn [x]
;;                                 (vec (repeat x {:b s/Int})))
;;                               (gen/elements [1 2 3])))})

;; (g/generate (asdf))

;; (def phrase-test
;;   [
;;    {:pitches [s/Int]
;;     :part (s/enum :lower :upper)
;;     :duration (s/enum 1/16 1/8 1/4)
;;     }
;;    ])

;; (def chords
;;   [[0] [0 2] [0 2 4] [0 2 4 5] [0 4 7]])

;; (g/generate {:a (apply s/enum chords)})

;; (defn asdf
;;   []
;;   (gen/fmap (fn [cnt]
;;               (vec (repeat cnt "x")))
;;             (gen/elements [1 3])))

;; (defn qwer [cnt]
;;   (gen/sample (gen/fmap (fn [cnt]
;;                           (vec (repeatedly cnt
;;                                            (partial gen/sample (asdf)))))
;;                         (gen/elements [1 2 3]))
;;               cnt))

;; (gen/sample (gen/fmap (fn [cnt] (repeat cnt 42))
;;                       (gen/elements [1 1 1 1 3])))

;; (gen/sample (gen/fmap note/make-note
;;                       (g/generator phrase-data))
;;             2)


;; (deftest test-extend-phrases
;;   (let [diss-fn-params {:max-count 100
;;                         :max-lingering 300
;;                         :diss-params [0 1 2]}
;;         phrases [
;;                  [(make-note {:pitch 8})]
;;                  [(make-note {:pitch 8})]
;;                  ]
;;         ]
;;     (is (sut/extend-phrases diss-fn-params [] phrases)
;;         phrases)))

;; (let [diss-fn-params {:max-count 100
;;                       :max-lingering 300
;;                       :diss-params [0 1 2]}
;;       phrases [
;;                [[(make-note {:pitch 8})]]
;;                [[(make-note {:pitch 3})]]
;;                ]
;;       ]
;;   (= phrases
;;   (sut/extend-phrases diss-fn-params [] phrases))

;; planning?

;; (defn get-pitches
;;   [m]
;;   (clojure.walk/prewalk (fn [x] (if (map? x) (:pitch x) x))
;;                         m))

;; (test/deftest extend-phrase
;;   (let [phrase (->> [
;;                      {:pitch [0]}
;;                      {:pitch [4]}
;;                      {:pitch [7]}
;;                      {:pitch [9]}
;;                      ]
;;                     (mapv utils/make-chord-from-pitch-vector-params))
;;         result (get-pitches (chord-seq/extend-chords phrase))]
;;         (test/is (= (get-pitches phrase)
;;                     [[0] [4] [7] [9]]))
;;         (test/is (= result
;;                     [[0 4 7 9]]))))

;; (test/deftest extend-phrase
;;   (let [phrase (->> [
;;                      {:pitch [0]}
;;                      {:pitch [4]}
;;                      {:pitch [7]}
;;                      {:pitch [9]}
;;                      ])
;;         result (chord-seq/extend-chords (fn [a b] true)
;;                                         concat
;;                                         phrase)]
;;         (test/is (= result
;;                     [{:pitch [0 4 7 9]}]))))
