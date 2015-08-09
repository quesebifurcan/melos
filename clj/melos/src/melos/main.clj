(ns melos.main
  (:require [melos.scores.compose-score :refer [compose]]
            [melos.utils :refer [export-to-json]]
            [schema.core :as s]))

(s/set-fn-validation! true)

(defn -main
  [output-path]
  (time
   (export-to-json output-path
                   (compose))))

(-main "/Users/fred/Desktop/score.json")

;; (require '[melos.chord-seq :as chord-seq])

;; chord-seq/modify-durations

;; tools/scaled-dissonance-value

;; tools

