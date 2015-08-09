(ns score.main
  (:require [melos.utils :as utils]
            [schema.core :as s]
            [score.compose-score :refer [compose]]))

(s/set-fn-validation! true)

(defn -main
  [output-path]
  (time
   (utils/export-to-json output-path
                         (compose))))
