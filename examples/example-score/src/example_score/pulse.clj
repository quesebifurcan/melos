(ns example-score.pulse
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
  (let [group (gensym "G__")
        notation {:type :pulse
                  :subdivisions 7
                  :pattern [0 1 0 0]}]
    (map (fn [chord]
           (->> chord
                (chord/set-chord-key :group group)
                (chord/set-chord-key :notation notation)))
         phrase)))

(defn pulse
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
         (map apply-notations)
         )))


