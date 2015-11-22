(ns melos.score.main
  (:require [melos.lib.utils :as utils]
            [melos.score.compose-score :as compose-score]
            clojure.edn
            [schema.core :as s]))

(s/set-fn-validation! true)

(defn -main
  [output-path analysis-dir config-file-path]
  (let [{:keys [sessions composition-fn rerender-sessions? compose-score?]}
         (eval (clojure.edn/read-string (slurp config-file-path)))]
    (when rerender-sessions?
      (doall (compose-score/calc-all-sessions analysis-dir sessions)))
    (when compose-score?
      (utils/export-to-json output-path
                            (composition-fn analysis-dir sessions)))))
