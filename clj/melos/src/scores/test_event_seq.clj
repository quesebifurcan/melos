(ns scores.test-event-seq
  (:require [scores.event-seqs :refer [pendulum-1 make-melody]]
            [melos.tools.rtm :refer [calculate-result update-children]]
            [melos.tools.make-note :refer [make-note]]
            [melos.tools.utils :refer [export-to-json
                                       mapply]]
            [melos.tools.l-systems :refer [lindenmayer]]
            ))

(defn export-single-event-seq [events]
  (export-to-json "/Users/fred/Desktop/score.json"
                  [[{:part-name :upper
                     :events ((comp update-children calculate-result) events)}]]))

(defn unfold-events
  [m]
  (->> (map cycle (vals m))
       (apply map vector)
       (map (fn [x] (zipmap (keys m) x)))
       (map #(mapply make-note %))))

(let [c {:pitch [1 2 3]
         :dissonance-contributor? [false]
         :allow-extension? [false false true]
         :duration [2/4 3/4 7/16 5/4]}]
  (take 10 (unfold-events c)))
