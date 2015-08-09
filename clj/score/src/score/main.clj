(ns score.main
  (:require
   [schema.core :as s]
   [score.compose-score :refer [compose]]
   [melos.utils
    [utils :as utils]]))

(s/set-fn-validation! true)

(defn -main
  [output-path]
  (time
   (utils/export-to-json output-path
                   (compose))))
