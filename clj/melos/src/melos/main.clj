(ns melos.main
    (:require [schema.core :as s]
              [melos.tools.utils :refer [export-to-json]]
              [melos.scores.segments.segment :as segment]))

(s/set-fn-validation! true)

(defn -main
  [output-path]
  (time
   (export-to-json output-path
                   (segment/compose))))

(-main "/Users/fred/Desktop/score.json")
