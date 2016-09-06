(ns example-score.multi
  (:require [melos
             [chord :as chord]
             [chord-seq :as chord-seq]
             [note :as note]
             [utils :as utils]
             [schemas :as ms]]))

(defn slope [a b]
  (fn [groups]
    (mapcat (fn [count_] (utils/apply-slope count_ a b)) groups)))

(defn multi
  [{:keys [parts phrases] :as m}]
  (let [params (merge (select-keys m (keys chord/chord-default))
                      (select-keys m (keys (note/note-default)))
                      {:phrase-end?
                       ((slope false true)
                        (map count (cycle phrases)))})]
    (map (fn [phrase]
           (let [phrase-group (gensym "G__")]
           (map (fn [chords params]
                  (let [group (gensym "G__")]
                    (reduce
                     chord-seq/merge-chords
                     (map (fn [part chord]
                            (chord/make-chord
                             (merge {:part part
                                     :group phrase-group
                                     :pitches chord}
                                    params)))
                          parts
                          chords))))
                phrase
                (utils/unfold-parameters params)
                )))
         (cycle phrases))))
