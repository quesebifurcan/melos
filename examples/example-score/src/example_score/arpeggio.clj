(ns example-score.arpeggio
  (:require [melos
             [chord :as chord]
             [note :as note]
             [utils :as utils]
             [schemas :as ms]]))

(defn slope [a b]
  (fn [groups]
    (mapcat (fn [count_] (utils/apply-slope count_ a b)) groups)))

(defn add-tie
  [phrase]
  (let [group (gensym "G__")]
    (map (fn [chord] (chord/set-chord-key :group group chord)) phrase)))

(defn arpeggio
  [{:keys [phrases part-name transposition durations]}]
  (let [groups (map count phrases)
        chords (->> (apply concat phrases)
                    (utils/transpose-all transposition))
        phrase-end ((slope false true) groups)]
    (->> {:pitches chords
          :part [part-name]
          :duration durations
          :phrase-end? phrase-end}
         (utils/unfold-parameters)
         (map chord/make-chord)
         (take (count chords))
         (utils/partition-groups :phrase-end?)
         (map add-tie)
         )))

