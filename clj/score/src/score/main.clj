(ns score.main
  (:require [melos.utils :as utils]
            [schema.core :as s]
            [score.compose-score :as compose-score-1]))

(s/set-fn-validation! true)

(defn -main
  [output-path]
  (time
     (utils/export-to-json output-path
                           (compose-score-1/compose))))

;; (-main "/Users/fred/Desktop/score.json")
