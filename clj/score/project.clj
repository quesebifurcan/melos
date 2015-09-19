(defproject score "0.1.0-SNAPSHOT"
  :main score.main
  :global-vars {*print-length* 25}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [prismatic/schema "0.4.0"]
                 [melos "0.1.0-SNAPSHOT"]])
