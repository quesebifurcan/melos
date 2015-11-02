(defproject score "0.1.0-SNAPSHOT"
  :main score.main
  :global-vars {*print-length* false}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [prismatic/schema "0.4.0"]
                 [progressbar "0.0.2"]
                 [org.clojure/tools.namespace "0.2.11"]
                 [me.raynes/conch "0.8.0"]
                 [melos "0.1.0-SNAPSHOT"]])
