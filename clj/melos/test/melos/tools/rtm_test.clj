(ns melos.tools.rtm-test
  (:require [clojure.test :refer :all]
            [melos.rhythm-tree.rtm :refer :all]))

(deftest test-all-children-same-pitch?
  (let [node {:children [{:events [{:pitch 2}
                                   {:pitch 4}]}
                         {:events [{:pitch 4}
                                   {:pitch 2}]}]}]
  (is (all-children-same-pitch? node)))

  (let [node {:children [{:events [{:pitch 2}
                                   {:pitch 4}]}
                         {:events [{:pitch 3}
                                   {:pitch 2}]}]}]
  (is (not (all-children-same-pitch? node))))

  (let [node {:children [{:events [{:pitch 4}]}
                         {:events [{:pitch 4}
                                   {:pitch 2}]}]}]
  (is (not (all-children-same-pitch? node))))

  (let [node {:children [{:events [{:pitch 2}]}
                         {:events [{:pitch 2}]}]}]
  (is (all-children-same-pitch? node))))
