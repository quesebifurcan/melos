(ns melos.score.main
  (:require [melos.lib.utils :as utils]
            [schema.core :as s]))

(s/set-fn-validation! true)

(defn -main
  [output-path composition-fn sessions]
  (time
     (utils/export-to-json output-path
                           (composition-fn sessions))))
