(ns melos.main
    (:require [schema.core :as s]
              [melos.tools.utils :refer [export-to-json]]
              [melos.scores.segments.segment-a :as segment-a]))

(s/set-fn-validation! true)

(defn -main
  [output-path]
  (time
   (export-to-json output-path
                   (segment-a/compose))))

(-main "/Users/fred/Desktop/score.json")
