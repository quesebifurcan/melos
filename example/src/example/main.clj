(ns example.main
  (:require [clojure.java.shell :as shell]
            [melos
             [chord :as chord]
             [chord-seq :as chord-seq]
             [measure :as measure]
             [note :as note]
             [part :as part]
             [schemas :as ms]
             [utils :as utils]])
  (:import [melos.schemas Chord Note]))

(def melody
  (let [chords [{:pitches [0] :part :voice-1}
                {:pitches [1] :part :voice-2}
                {:pitches [2] :part :voice-3}]]
    (map chord/make-chord chords)))

(def measure-1
  (let [durations {1              [3/4 [1/2 :stretch]]
                   3/4            [[1/2 :stretch] 1/4]
                   [1/2 :stretch] [1/2 1/4]
                   1/2            [1/4 1/4]
                   1/4            [1/8 1/8]
                   1/8            [1/16 1/16]}]
    (measure/make-rtm-tree durations 1)))

(defn rtm-tree-zipper [measures] (measure/rtm-tree-zipper {:root true :children measures}))

(defn cycle-measures
  [dur measures]
  (if (> dur 0)
    (let [head (first measures)]
      (cons head
            (cycle-measures (- dur (:sum-of-leaves-duration head))
                            (utils/rotate measures))))))

(defn make-rtm-tree
  [measures event-seq]
  (-> (measure/insert-chords event-seq (rtm-tree-zipper measures)) :children))

(defn sum-by-key [k xs] (apply + (map k xs)))

(defn extend-last
  [duration xs]
  (concat (butlast xs)
          [(update (last xs) :duration #(+ % duration))]))

(defn make-voice
  [{:keys [events part-name measure-list]}]
  (let [event-seq         (part/filter-chords part-name events)
        total-duration    (sum-by-key :duration event-seq)
        measures          (cycle-measures total-duration measure-list)
        measures-duration (sum-by-key :sum-of-leaves-duration measures)
        overhang          (- measures-duration total-duration)
        extended          (extend-last overhang event-seq)]
    {:type :Voice
     :name part-name
     :measures (->> ((chord-seq/simplify-event-seq) extended)
                    (make-rtm-tree measures))}))

(defn compose-section
  [{:keys [melody
           handle-dissonance-fn
           tempo
           measure-list
           merge-horizontally-fn]}]
  (let [events (->> melody
                    handle-dissonance-fn
                    (chord-seq/merge-horizontally merge-horizontally-fn))
        voices (map make-voice [{:part-name :voice-1
                                 :events events
                                 :measure-list measure-list}
                                {:part-name :voice-2
                                 :events events
                                 :measure-list measure-list}
                                {:part-name :voice-3
                                 :events events
                                 :measure-list measure-list}])
        template' (fn [tempo [voice-1 voice-2 voice-3]]
                    {:type :Section
                     :staves [{:type :Staff
                               :tempo tempo
                               :name :a
                               :voices [voice-1]}
                              {:type :Staff
                               :name :b
                               :voices [voice-2]}
                              {:type :Staff
                               :name :c
                               :voices [voice-3]}]})]
    (template' tempo voices)))

(def dissonance-map
  {0 0,
   1 6,
   2 5,
   3 3,
   4 2,
   5 1,
   6 4})

(defn dissonance-score
  [intervals]
  (->> intervals
       chord/inversion-equivalent-pitchclasses
       (map dissonance-map)
       (apply +)))

(defn handle-dissonance-fn'
  [limit]
  (fn f
    ([xs]
     (f [] xs))
    ([a b]
     (chord/reduce-dissonance dissonance-score
                              limit
                              (chord-seq/merge-chords a b)))))

(defn handle-dissonance-fn
  [dissonance-limit]
  (fn [events]
    (reductions (handle-dissonance-fn' dissonance-limit) events)))

(defn sections
  []
  [{:handle-dissonance-fn (handle-dissonance-fn [0 2 4 5])
    :tempo 144
    :melody melody
    :measure-list [measure-1]
    :merge-horizontally-fn (fn [_ _] false)}])

(defn render
  []
  (let [filename "output/example_1.json"]
    (utils/export-to-json
     filename
     {:type :Score
      :title "test"
      :author "anonymous"
      :sections (mapv compose-section (sections))})
    (shell/sh "scripts/show.sh" filename)))
