(ns melos.tools.rtm
  (:require [schema.core :as s]
            [melos.tools.schemata :as ms]))

;; Cycle measures across total duration.

(s/defn get-melodic-event
  :- ms/Note
  [vertical-moment :- ms/VerticalMoment]
  (->> vertical-moment
       (filter #(= (:count %) 0))
       (first)))

(defn is-active-node?
  [node]
  (and (map? node)
       (contains? node :event)))

(defn get-duration-of-node
  [node]
  (let [[num denom] (:w-duration node)]
    (/ num denom)))

(defn accumulate-node-dur!
  [dur-atom node]
  (if (is-active-node? node)
    (swap! dur-atom + (get-duration-of-node node))
    node))

(defn get-nested-measure-dur
  [measure]
  (let [dur-atom (atom 0)]
    (clojure.walk/prewalk (partial accumulate-node-dur! dur-atom)
                          measure)
    @dur-atom))

(defn get-next-measure
  ([measure-seq dur]
   (get-next-measure measure-seq dur []))
  ([measure-seq dur coll]
   (let [head (first measure-seq)
         head-dur (get-nested-measure-dur head)]
     (cond (>= head-dur dur)
           (concat coll head)
           :else
           (get-next-measure (rest measure-seq)
                             (- dur head-dur)
                             (concat coll head))))))

(defn cycle-measures-across-duration
  [measure-seq duration]
  (get-next-measure (cycle [measure-seq])
                    duration))

;; Build rhythmic tree.

(defn get-melodic-durations
  [vertical-moments]
  (map (comp :duration get-melodic-event)
       vertical-moments))

(defn duration-sum
  [events]
  (reduce + 0 (get-melodic-durations events)))

(defn init-rtm-tree
  [duration measures]
  {:duration :?
   :top-level true
   :children (cycle-measures-across-duration
              measures
              duration)})

(declare insert-events)

(defn extend-last
  [duration events]
  (let [extended (->> (last events)
                      (map (fn [event]
                             (update-in event
                                        [:duration]
                                        (fn [x] (+ x duration)))))
                      (conj []))]
    (concat (butlast events)
            extended)))

(defn make-r-tree
  [measures events]
  (let [total-dur (duration-sum events)
        rtm-tree (init-rtm-tree total-dur measures)
        rtm-tree-dur (get-nested-measure-dur rtm-tree)
        dur-diff (- rtm-tree-dur total-dur)
        events (extend-last dur-diff events)]
    (->> (insert-events rtm-tree events)
         ((fn [x] {:children (:children x)})))))

;; Insert events into rhythmic tree.

(defn decrement-duration
  [vertical-moment]
  (map (fn [x] (update-in x
                          [:duration]
                          (fn [y] (- y 1/8))))
       vertical-moment))

(defn update-events
  [events]
  ;; TODO: specify which duration to subtract and check that no event
  ;; is shorter than this duration.
  (cond
    (empty? events)
    nil
    (< (:duration (get-melodic-event (first events))) 1/8)
    (if (empty? (rest events))
      nil
      (let [new-first (first (rest events))
            new-first (decrement-duration new-first)]
        (concat [new-first]
                (rest (rest events)))))
    :else
    (let [new-first (decrement-duration (first events))]
      (into [] (concat [new-first]
                       (rest events))))))

(defn current-event [events]
  (if (empty? events)
    nil
    (first events)))

(defn forward-time!
  [events-atom node]
  (if (and (map? node)
           (contains? node :event))
    (do (swap! events-atom update-events)
        (assoc node :events (current-event @events-atom)))
    node))

(defn insert-events [rtm-tree events]
  (let [events-atom (atom events)]
    (clojure.walk/prewalk (partial forward-time! events-atom)
                          rtm-tree)))

;; If all :children of node have the same pitch, move them to node.

(defn pitchset-of-event
  [event]
  (set (map :pitch event)))

(defn non-empty-pitchsets
  [node]
  (let [children (:children node)
        events (map :events children)]
    (filter #(not (empty? %))
            (map pitchset-of-event events))))

(defn all-children-same-pitch?
  [node]
  (let [pitch-sets (non-empty-pitchsets node)]
    (and (every? #(= % (first pitch-sets))
                 (rest pitch-sets))
         (not (nil? (first pitch-sets)))
         (not (empty? pitch-sets))
         (> (count pitch-sets) 1))))

(defn merge-tied
  [node]
  (if (all-children-same-pitch? node)
    (assoc node :events
           ((comp :events first :children) node))
    node))

(defn merge-all-tied
  [measure]
  (clojure.walk/postwalk merge-tied measure))

;; TODO: join adjacent tuplets where all notes have the same ids.
;; TODO: convert tripleted two-note groups with equal length to eigth notes.
;; TODO: clojure.zip
;; TODO: attach time signatures.
;; TODO: filter parts.
;; TODO: insert FixedDurationTuplet when needed.

;; TODO: clean up treatment of node/measure.

;; TODO: test with real events. Skip onsets?

;;-------------------------------------------------------------------
;; Convenience functions for creating rhythmic trees.
