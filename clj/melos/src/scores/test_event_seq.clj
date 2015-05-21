(ns scores.test-event-seq
  (:require [scores.event-seqs :refer [pendulum-1 make-melody]]
            [melos.tools.rtm :refer [calculate-result update-children]]
            [melos.tools.make-note :refer [make-note]]
            [melos.tools.utils :refer [export-to-json
                                       mapply]]
            [melos.tools.l-systems :refer [lindenmayer]]
            ))

(defn compose-single-line
  [events]
  ((comp update-children calculate-result) events))

(defn export-single-event-seq [part-name events]
  (let [result (compose-single-line events)]
    (export-to-json "/Users/fred/Desktop/score.json"
                    [[{:part-name part-name :events result}]])))

(defn parse-params
  [x]
  (if (list? x)
    (repeat (last x) (first x))
    [x]))

(defn unfold-events
  [m]
  (let [f (:fn m)
        m (dissoc m :fn)]
  (->> (map (fn [x] (mapcat parse-params x))
            (vals m))
       (map cycle)
       (apply map vector)
       (map (fn [x] (zipmap (keys m) x)))
       (map f))))

(defn split-if-chord
  [events]
  (clojure.walk/prewalk
   #(if (and (map? %)
             (vector? (:pitch %)))
      (let [pitches (:pitch %)]
        (into [] (map (fn [x] (assoc % :pitch x)) pitches)))
      %)
   events))

(def alternating-pitch-rest
  {:pitch (mapcat (fn [x] [x "rest"])
                  (range 0 10))
   :dissonance-contributor? [false]
   :part [:upper]
   :fn (fn [x] [(mapply make-note x)])
   :duration [1/4 2/4]})

(def chords
  {:pitch [[0 2 4] [3 10 14]]
   :dissonance-contributor? [false]
   :part [:upper]
   :fn (fn [x] (->> (mapply make-note x)
                    (split-if-chord)
                    ;; (map split-if-chord)))
                    ))
   :duration [1/4 2/4]})

(->> (take 20 (unfold-events chords))
     (export-single-event-seq :upper))

