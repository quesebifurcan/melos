(ns scores.test-event-seq
  (:require [scores.event-seqs :refer [pendulum-1 make-melody]]
            [rtm :refer [calculate-result update-children]]
            [melos.tools.utils :refer [export-to-json]]
            [melos.tools.l-systems :refer [lindenmayer]]
            ))

(defn lindenmayer-3
  []
  (let [pitches (lindenmayer {5 [2 5]
                              2 [5]}
                             5
                             [2])]
    pitches))

(lindenmayer-3)

  

;; (defn export-single-event-seq [events]
;;   (export-to-json "/Users/fred/Desktop/score.json"
;;                   [[{:part-name :upper
;;                      :events ((comp update-children calculate-result) events)}]]))

;; (export-single-event-seq (take 100 (pendulum-1 :upper)))


;; TODO: rests as part of seq?
