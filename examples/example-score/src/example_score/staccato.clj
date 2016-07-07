(ns example-score.staccato
  (:require [melos
             [chord :as chord]
             [note :as note]
             [utils :as utils]
             [schemas :as ms]]))

(defn slope [a b]
  (fn [groups]
    (mapcat (fn [count_] (utils/apply-slope count_ a b)) groups)))

(defn make-chord-rest-pair
  [opts]
  (let [duration (:duration opts)
        note-duration (:note-duration opts)
        diff_ (- duration note-duration)]
    (if (= diff_ 0)
      [(chord/make-chord opts)]
      [(chord/make-chord (merge opts {:duration 1/8 :phrase-end? false}))
       (chord/make-chord (merge opts {:is-rest? true :duration 1/8}))])))

(defn staccato
  [{:keys [phrases part-name transposition durations note-durations]}]
  (let [groups (map #(* 1 (count %)) phrases)
        chords (->> (apply concat phrases)
                    (utils/transpose-all transposition))
        phrase-end ((slope false true) groups)]
    (->> {:pitches chords
          :part [part-name]
          :duration durations
          :note-duration note-durations
          :phrase-end? phrase-end}
         (utils/unfold-parameters)
         (take (count chords))
         (mapcat make-chord-rest-pair)
         (utils/partition-groups :phrase-end?))))
