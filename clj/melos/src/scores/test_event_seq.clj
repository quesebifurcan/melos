(ns scores.test-event-seq
  (:require [melos.tools.rtm :refer [calculate-result update-children]]
            [melos.tools.make-note :refer [make-note]]
            [melos.tools.utils :refer [export-to-json
                                       mapply]]
            [melos.tools.l-systems :refer [lindenmayer]]
            ))

(defn compose-single-line
  [events]
  ((comp update-children calculate-result)
   (map first events)))

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
        partition-fn (:partition m)
        m (dissoc m :fn :partition)]
  (->> (map (fn [x] (mapcat parse-params x))
            (vals m))
       (map cycle)
       (apply map vector)
       (map (fn [x] (zipmap (keys m) x)))
       (map f)
       (partition-fn))))

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
   :partition (fn [x] (partition 1 x))
   :duration [1/4 2/4]})

(def chords
  {:pitch ['([0 2 4] 1) [3 10 14]]
   :dissonance-contributor? [false]
   :part [:upper]
   :fn (fn [x] (->> (mapply make-note x)
                    (split-if-chord)
                    ;; (map split-if-chord)))
                    ))
   :partition (fn [x] (partition 1 x))
   :duration [1/4 2/4]})

(->> (take 5 (unfold-events alternating-pitch-rest))
;; (->> (take 5 (unfold-events chords))
     ;; (map (fn [x] [x]))
     ;; (export-single-event-seq :upper)
     )

