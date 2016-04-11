(ns melos.lib.chord-test
  (:require [melos.lib.chord :as sut]
            [melos.lib.schemas :as ms]
            [clojure.test :as test]
            [clojure.test.check :as tc]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [schema.core :as s]
            [schema-generators.complete :as c]
            [schema-generators.generators :as g]))

(def property (prop/for-all [i gen/int] (= i i)))
(defspec chord 100 property)
