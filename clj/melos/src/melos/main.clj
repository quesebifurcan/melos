(ns melos.main
    (:require [schema.core :as s]
              [melos.tools.utils :refer [export-to-json]]
              [melos.scores.compose-score :refer [compose]]))

(s/set-fn-validation! true)

(defn -main
  [output-path]
  (time
   (export-to-json output-path
                   (compose))))

(-main "/Users/fred/Desktop/score.json")
