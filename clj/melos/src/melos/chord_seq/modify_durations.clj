(ns melos.chord-seq.modify-durations
  (:require [melos.note.make-note :refer [make-note]]
            [melos.schemas.schemas :as ms]
            [schema.core :as s]))

(defn pairwise-mod
  [events tests coll]
  (let [pair (take 2 events)]
    (cond (empty? pair)
          coll
          :else
          (let [result (drop-while
                        nil?
                        (map #(% pair) tests))]
            (if (empty? result)
              (pairwise-mod (rest events)
                            tests
                            (concat coll [(first events)]))
              (pairwise-mod (drop 2 events)
                            tests
                            (concat coll (into [] (first result)))))))))

(s/defn modify-durations
  :- [ms/VerticalMoment]
  [events :- [ms/VerticalMoment]
   mod-fns :- [s/Any]]
  (pairwise-mod events mod-fns []))
