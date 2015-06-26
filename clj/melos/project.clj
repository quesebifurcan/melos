(defproject melos "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.7.0-alpha5"]
                 [org.clojure/algo.generic "0.1.2"]
                 [org.clojure/data.json "0.2.5"]
                 [org.clojure/math.combinatorics "0.0.8"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.clojure/tools.trace "0.7.8"]
                 [prismatic/plumbing "0.4.2"]
                 [com.taoensso/timbre "3.4.0"]
                 [prismatic/schema "0.4.0"]]
  :main melos.main
  :global-vars {*print-length* 20}
  :plugins [[lein-marginalia "0.8.0"]])

