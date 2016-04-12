(ns melos.lib.rhythm-tree
  (:require [melos.lib
             [chord :as chord]
             [chord-seq :as chord-seq]
             [schemas :as ms]]
            [schema.core :as s]))

;; Cycle measures across total duration.

(defn is-active-node?
  [node]
  (and (map? node)
       (contains? node :event)))

(defn get-duration-of-node
  [{:keys [written-duration]}]
  (let [[num denom] written-duration]
    (/ num denom)))

(defn increment-node-duration
  [dur-atom node]
  (if (is-active-node? node)
    (swap! dur-atom + (get-duration-of-node node))
    node))

(defn get-measure-duration
  [measure]
  (let [dur-atom (atom 0)
        fn_ (partial increment-node-duration dur-atom)]
    (clojure.walk/prewalk fn_ measure)
    @dur-atom))

(defn get-next-measure
  ([measure-seq dur]
   (get-next-measure measure-seq dur []))
  ([measure-seq dur coll]
   (let [head (first measure-seq)
         head-dur (get-measure-duration head)]
     (cond (>= head-dur dur)
           (concat coll head)
           :else
           (get-next-measure (rest measure-seq)
                             (- dur head-dur)
                             (concat coll head))))))

(defn cycle-measures-across-duration
  [measure-seq duration]
  (get-next-measure (cycle [measure-seq]) duration))

;; Build rhythmic tree.

(defn init-rtm-tree
  [duration measures]
  {:children (cycle-measures-across-duration measures duration)})

(declare insert-events)

(defn extend-last
  [duration events]
  (let [extended (->> (last events)
                      (map (fn [event]
                             (update-in event
                                        [:duration]
                                        (fn [x] (+ x duration)))))
                      (vector))]
    (concat (butlast events)
            extended)))

(defn make-r-tree
  [measures events]
  (let [total-dur (chord-seq/sum-melodic-durations events)
        rtm-tree (init-rtm-tree total-dur measures)
        rtm-tree-dur (get-measure-duration rtm-tree)
        dur-diff (- rtm-tree-dur total-dur)
        events (extend-last dur-diff events)]
    (select-keys (insert-events rtm-tree events) [:children])))

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
    (< (:duration (chord/get-melodic-event (first events))) 1/8)
    (when-not (empty? (rest events))
      (let [new-first (first (rest events))
            new-first (decrement-duration new-first)]
        (concat [new-first] (rest (rest events)))))
    :else
    (let [new-first (decrement-duration (first events))]
      (vec (concat [new-first]
                   (rest events))))))

(defn current-event [events]
  (when-not (empty? events) (first events)))

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

(defn non-empty-pitchsets
  [node]
  (let [children (:children node)
        events (map :events children)]
    (remove empty? (map chord/pitchset events))))

(defn all-children-same-pitch?
  [node]
  (let [pitch-sets (non-empty-pitchsets node)]
    (and (every? #(= % (first pitch-sets))
                 (rest pitch-sets))
         (not (nil? (first pitch-sets)))
         (seq pitch-sets)
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
