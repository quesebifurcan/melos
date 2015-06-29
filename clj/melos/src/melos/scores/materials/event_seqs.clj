(ns melos.scores.materials.event-seqs
  (:require [melos.tools.utils :as utils]))

(defn ascending
  [part-name transposition]
  (let [pitches (concat (range 0 13)
                        (range 13 0 -1))]
    {:pitch (->> pitches
                 (utils/transpose transposition)
                 (map utils/maybe-vec)
                 (utils/cyclic-repeats [1]))
     :part [part-name]
     :fn utils/make-chord-from-pitch-vector-params
     :partition (partial utils/cyclic-partition [1 2 1 2 3 1 2 3 4])
     :duration [1/4 1/4 1/4 1/4]}))

(defn expanding
  [part-name transposition]
  (let [ranges [(range 0 7)
                 (range 0 10)
                 (range 0 7)
                 (range 0 10)
                 (range 0 13)
                 (range 0 7)
                 (range 0 10)
                 (range 0 17)]
        basic-partition (map count ranges)
        pitches (apply concat ranges)]
    {:pitch (->> pitches
                 (utils/transpose transposition)
                 (map utils/maybe-vec)
                 (utils/cyclic-repeats [1]))
     :part [part-name]
     :fn utils/make-chord-from-pitch-vector-params
     ;; TODO: double partition using basic-partition
     :partition (partial utils/cyclic-partition [1 2 1 2 3 1 2 3 4])
     :duration [1/4 1/4 1/4 1/4]}))

(defn fifth-octave-arpeggio
  [part-name transposition]
  {:pitch (->> [0 12 7 0 7 12 19 12 7]
               (utils/transpose transposition)
               (map utils/maybe-vec))
   :part [part-name]
   :fn utils/make-chord-from-pitch-vector-params
   :partition (partial utils/cyclic-partition [1])
   :duration [1/4]})

(defn fifth-octave-arpeggio-2
  [part-name transposition]
  {:pitch (->> [0 12 24 12 0 7 12 7 19 24 12 0 12 0 12 0 12]
               (utils/transpose transposition)
               (map utils/maybe-vec))
   :part [part-name]
   :fn utils/make-chord-from-pitch-vector-params
   :partition (partial utils/cyclic-partition [2 3 2 1 1 1 2 3])
   :duration [1/4 1/4 1/4 1/4 3/4]})

(defn descending-slow
  [part-name transposition]
  (let [pitches (range 0 -5 -1)]
    {:pitch (->> pitches
                 (utils/transpose transposition)
                 (map utils/maybe-vec)
                 (utils/cyclic-repeats [2 3 4]))
     :part [part-name]
     :fn utils/make-chord-from-pitch-vector-params
     :partition (partial utils/cyclic-partition [1])
     :duration [1/4]}))

(defn diatonic-cluster-arpeggio
  [part-name transposition]
  (let [pitches [0 2 4 5 4 2]]
    {:pitch (->> pitches
                 (utils/transpose transposition)
                 (map utils/maybe-vec))
     :part [part-name]
     :fn utils/make-chord-from-pitch-vector-params
     :partition (partial utils/cyclic-partition [1 2 1 2 3 2])
     :duration [1/4]}))

(defn chords-contracting
  [part-name transposition]
  {:pitch (->> [[0 2 4 5 6]
                [0 2 4 5]
                [0 2 4]
                [0 2]]
               (map (partial utils/transpose transposition)))
   :part [part-name]
   :fn utils/make-chord-from-pitch-vector-params
   :partition (partial utils/cyclic-partition [1 2 1 2])
   :duration [1/4 1/4 1/4 1/4 1/4 5/4 1/4 1/4 1/4 3/4]})

(defn organ
  []
  {:upper/a
   (utils/unfold-events (ascending :upper -1))
   :lower/a
   (utils/unfold-events (fifth-octave-arpeggio :lower -3))
   :ped/a
   (utils/unfold-events (descending-slow :ped -17))})

