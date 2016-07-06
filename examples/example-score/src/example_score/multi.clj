(ns example-score.multi
  (:require [melos
             [chord :as chord]
             [chord-seq :as chord-seq]
             [note :as note]
             [utils :as utils]
             [schemas :as ms]]))

(defn make-note*
  [group]
  (fn [voice pitch]
    (note/make-note {:part voice :group group :pitch pitch})))

(defn multi
  [{:keys [tempo duration phrase-end? voices pitches] :as m}]
  (let [parts (set voices)
        groups (map count pitches)
        chords (apply concat pitches)
        chord-params (-> m
                         (select-keys (keys chord/chord-default))
                         utils/unfold-parameters)]
    (->> (map (fn [params chord]
                (let [events (map (make-note* (gensym "G__")) voices chord)]
                  (-> (chord/make-chord params)
                      (assoc :events events))))
              chord-params
              chords)
         (utils/cyclic-partition groups)
         cycle)))
