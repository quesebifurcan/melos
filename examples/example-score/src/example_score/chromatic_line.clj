(ns example-score.chromatic-line
  (:require [melos
             [chord :as chord]
             [note :as note]
             [utils :as utils]
             [schemas :as ms]]))

(defn slope [a b]
  (fn [groups]
    (mapcat (fn [count_] (utils/apply-slope count_ a b)) groups)))

(defn chromatic-line
  [{:keys [phrases part-name transposition durations check-dissonance]}]
  (let [groups (map count phrases)
        chords (->> (apply concat phrases)
                    (utils/transpose-all transposition))
        phrase-end ((slope false true) groups)]
    (->> {:pitches chords
          :part [part-name]
          :duration durations
          :check-dissonance check-dissonance
          ;; :merge-left? [true]
          ;; :merge-right? [true]
          :phrase-end? phrase-end}
         (utils/unfold-parameters)
         ;; (map (fn [x] (if (= (first (:pitches x)) 23)
         ;;                (-> x
         ;;                    (assoc :is-rest? true)
         ;;                    (assoc :pitch 0)
         ;;                    ;; ((fn [y] [y]))
         ;;                    )

         ;;                x)))
         (map chord/make-chord)
         (utils/cyclic-partition groups)
         )))
