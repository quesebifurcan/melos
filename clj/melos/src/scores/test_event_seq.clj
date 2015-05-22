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

(defn apply-contour
  [pitches]
  (->> (map (fn [x] (rem x 12)) pitches)
       ;; (sort)
       ;; ((fn [x] (concat x (reverse (butlast (rest x))))))))
       ))

(defn transpose-motif-gradually
  [pitches]
  (let [transpositions (reductions + 0 (repeat 12 7))
        tails (map (fn [increment]
                     (map (fn [x] (+ x increment))
                          (rest pitches))) 
                   transpositions)
        result (map (fn [x] (concat [(first pitches)] x))
                    tails)]
    (->> (map apply-contour result)
         (flatten))))

(transpose-motif-gradually [0 2 7])

(def morph
  {:pitch (transpose-motif-gradually [0 12 14 2])
   :dissonance-contributor? [true]
   :part [:upper]
   :fn (fn [x] [(mapply make-note x)])
   :partition (fn [x] (partition 1 x))
   :duration [1/4 1/4]})

(->> (take 400 (unfold-events morph))
;; (->> (take 5 (unfold-events chords))
     ;; (map (fn [x] [x]))
     (export-single-event-seq :upper)
     )

