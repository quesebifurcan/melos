(ns scores.test-event-seq
  (:require [scores.event-seqs :refer [pendulum-1 make-melody]]
            [melos.tools.rtm :refer [calculate-result update-children]]
            [melos.tools.make-note :refer [make-note]]
            [melos.tools.utils :refer [export-to-json
                                       mapply]]
            [melos.tools.l-systems :refer [lindenmayer]]
            ))

(defn compose-single-line
  [events]
  ((comp update-children calculate-result) events))

(defn export-single-event-seq [part-name events]
  (let [result (compose-single-line events)]
    (export-to-json "/Users/fred/Desktop/score.json"
                    [[{:part-name part-name :events result}]])))

(defn parse-params
  [x]
  (if (vector? x)
    (repeat (last x) (first x))
    [x]))

(defn unfold-events
  [m]
  (->> (map (fn [x] (mapcat parse-params x))
            (vals m))
       (map cycle)
       (apply map vector)
       (map (fn [x] (zipmap (keys m) x)))
       (map #(mapply make-note %))))

(let [c {:pitch (mapcat (fn [x] [x "rest"])
                        (range 0 10))
         :dissonance-contributor? [false]
         :part [:upper]
         :duration [1/4 2/4]}]
  (->> (take 40 (unfold-events c))
       (map (fn [x] [x]))
       (export-single-event-seq :upper)
       ))

;; (take 10 (pendulum-1 :upper))
