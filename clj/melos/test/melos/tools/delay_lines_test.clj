(ns melos.tools.delay-lines-test
  (:require [clojure.test :refer :all]
            [melos.chord-seq.delay-lines :refer :all]
            [melos.note.make-note :refer [make-note]]))

(defn- make-n-notes
  [n params]
  (take n (map (fn [param-map] (make-note param-map)) (cycle params))))

(deftest test-filter-dissonance-contributors
  (letfn [(test-fn [params]
            (filter-dissonance-contributors (make-n-notes 10 params)))]
    (is (empty?
         (test-fn [{:dissonance-contributor? false}])))
    (is (not (empty?
              (test-fn [{:dissonance-contributor? true}]))))))

;; simplify notation: give pitches and result (fn separately declared).

(deftest test-consonant?
  (let [vertical-moment (map
                         (fn [pitch] (make-note {:pitch pitch})) (range 10))]
    (are [x y] (= x y)
      false (consonant? vertical-moment 0)
      true (consonant? vertical-moment 1000)
      true (consonant? [] 0))))

(deftest test-zero-count
  (is (= false (zero-count? (make-note {:count 8}))))
  (is (= true (zero-count? (make-note {:count 0})))))

