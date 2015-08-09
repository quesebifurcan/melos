(ns melos.chord-seq.default-horizontal-merge
  (:require [schema.core :as s]
            [melos.schemas.schemas :as ms]))

;; Merge horizontally

(s/defn can-merge?
  :- s/Bool
  [curr :- ms/Chord
   next :- ms/Chord]
  (let [old-curr (filter #(> (:count %) 0) next)
        news (filter #(= (:count %) 0) next)]
    (and (= (count curr) (count old-curr))
         (every? #(:merge-left? %) news)
         (every? #(:merge-right? %) old-curr))))

(s/defn merge-elts
  :- ms/Chord
  [a :- ms/Chord
   b :- ms/Chord]
  (let [melodic-notes (filter #(= (:count %) 0) b)]
    (concat a melodic-notes)))

(s/defn maybe-merge
  :- [ms/Chord]
  ([events :- [ms/Chord]]
   (if (seq events)
     (maybe-merge (first events)
                  (rest events))))
  ([head :- ms/Chord
    events :- [ms/Chord]]
   (cond (empty? events)
         (list head)
         (can-merge? head (first events))
         (maybe-merge (merge-elts head
                                  (first events))
                      (rest events))
         :else
         (cons head (maybe-merge events)))))

