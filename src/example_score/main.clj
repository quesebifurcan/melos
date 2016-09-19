(ns example-score.main
  (:require [clojure.java.shell :as shell]
            [clojure.algo.generic.functor :as functor]
            [melos
             [chord :as chord]
             [chord-seq :as chord-seq]
             [measure :as measure]
             [note :as note]
             [part :as part]
             [schemas :as ms]
             [utils :as utils]])
  (:import [melos.schemas Chord Note]))

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
     :measures (->> (chord-seq/simplify-event-seq extended)
                    (make-rtm-tree measures))}))

(defn voices-entry?
  [form]
  (and (vector? form)
       (= (first form) :voices)))

(defn compose-voices' [f form] [:voices (map f (second form))])

(defn compose-voices
  [template f]
  (clojure.walk/postwalk
   (fn [form]
     (if (voices-entry? form)
       (compose-voices' f form)
       form))
   template))

(defn compose-section
  [{:keys [event-seqs
           voice-seq
           dissonance-limit
           handle-dissonance-fn
           template
           tempo
           measure-list
           merge-horizontally-fn]}]
  (let [events (->> (chord-seq/cycle-event-seqs voice-seq event-seqs)
                    handle-dissonance-fn
                    (chord-seq/merge-horizontally merge-horizontally-fn))]
    (compose-voices (template tempo) #(make-voice {:events events
                                                   :part-name %
                                                   :measure-list measure-list}))))

(defn compose-phrase
  [params chords]
  (map (fn [chord] (chord/make-chord (merge params {:pitches chord})))
       chords))

(defn compose-phrases
  [params phrases]
  (map (partial compose-phrase params) phrases))

(def phrase
  [[[0]]
   [[2] [4]]
   [[5]]
   [[7] [9]]
   [[11]]
   [[12]]])

(defn voices
  []
  (->> [{:a (compose-phrases {:part :voice-1 :duration 1/4} phrase)}
        {:b (compose-phrases {:part :voice-2 :duration 1/4} phrase)}
        {:c (compose-phrases {:part :voice-3 :duration 1/4} phrase)}]
       (apply merge)
       (functor/fmap cycle)
       atom))

(def dissonance-map
  {0 0,
   1 21,
   2 5,
   3 3,
   4 2,
   5 1,
   6 8})

(defn apply-dissonance-filter?
  [chord]
  (and (:phrase-end? chord)
       (:check-dissonance chord)))

(defn handle-dissonance-fn'
  [limit]
  (fn f
    ([xs]
     (f [] xs))
    ([a b]
     (if (apply-dissonance-filter? b)
       (chord/reduce-dissonance dissonance-map
                                limit
                                (chord-seq/merge-chords a b))
       (chord-seq/merge-chords a b)))))

(defn handle-dissonance-fn
  [dissonance-limit]
  (fn [events]
    (reductions (handle-dissonance-fn' dissonance-limit) events)))

(defn sections
  []
  (let [event-seqs (voices)]
    [{:voice-seq (take 42 (cycle [:a :b :c :b :a :b]))
      :handle-dissonance-fn (handle-dissonance-fn [0 2 4 5])
      :tempo 144
      :template (fn [tempo] {:type :Section
                             :staves [{:type :Staff
                                       :tempo tempo
                                       :name :a
                                       :voices [:voice-1]}
                                      {:type :Staff
                                       :name :b
                                       :voices [:voice-2]}
                                      {:type :Staff
                                       :name :c
                                       :voices [:voice-3]}]})
      :event-seqs event-seqs
      :measure-list [measure-1]
      :merge-horizontally-fn (fn [_ _] false)}]))

(defn render
  []
  (let [filename "output/example_1.json"]
    (utils/export-to-json
     filename
     {:type :Score
      :title "test"
      :author "anonymous"
      :sections (mapv compose-section (sections))})
    (shell/sh "scripts/to_pdf.sh" filename)))
