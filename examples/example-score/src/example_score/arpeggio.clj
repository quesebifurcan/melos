(ns example-score.arpeggio
  (:require [melos
             [chord :as chord]
             [note :as note]
             [utils :as utils]
             [schemas :as ms]]))

(defn slope [a b]
  (fn [groups]
    (mapcat (fn [count_] (utils/apply-slope count_ a b)) groups)))

(defn apply-notations
  [phrase]
  (let [notation {:type :arpeggio}]
    (map (fn [chord]
           (->> chord
                (chord/set-chord-key :notation notation)))
         phrase)))

(defn arpeggio
  [{:keys [phrases part-name transposition durations]}]
  (let [groups (map count phrases)
        tie-group (gensym "G__")
        chords (->> (apply concat phrases)
                    (utils/transpose-all transposition))
        phrase-end ((slope false true) groups)]
    (->> {:pitches chords
          :part [part-name]
          :duration durations
          :check-dissonance [true]
          :phrase-end? phrase-end}
         (utils/unfold-parameters)
         (map chord/make-chord)
         (map #(chord/set-chord-key :group tie-group %))
         (take (count chords))
         (utils/partition-groups :phrase-end?)
         (map apply-notations)
         )))

