(ns melos.chord-seq.modify-durations
  (:require [melos.note.make-note :refer [make-note]]
            [schema.core :as s]
            [melos.schemas.schemas :as ms]))

(s/defn pairwise-mod
  :- [ms/Chord]
  [chords :- [ms/Chord]
   tests :- [s/Any]
   coll :- [ms/Chord]]
  (let [pair (take 2 chords)]
    (cond (empty? pair)
          coll
          :else
          (let [result (drop-while
                        nil?
                        (map #(% pair) tests))]
            (if (empty? result)
              (pairwise-mod (rest chords)
                            tests
                            (concat coll [(first chords)]))
              (pairwise-mod (drop 2 chords)
                            tests
                            (concat coll (into [] (first result)))))))))

(s/defn modify-durations
  :- [ms/Chord]
  [chords :- [ms/Chord]
   mod-fns :- [s/Any]]
  (pairwise-mod chords mod-fns []))

