(ns melos.score.main
  (:require [melos.lib.utils :as utils]
            [melos.score.compose-score :as compose-score-1]
            [schema.core :as s]))

(s/set-fn-validation! true)

(defn -main
  [output-path]
  (time
     (utils/export-to-json output-path
                           (compose-score-1/compose))))

;; (-main "/Users/fred/Desktop/score.json")
