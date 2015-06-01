(ns melos.tools.rtm
  (:require [schema.core :as s]
            [melos.tools.schemata :as ms]))

(s/defn get-melodic-event
  :- ms/Note
  [vertical-moment :- ms/VerticalMoment]
  (->> vertical-moment
       (filter #(= (:count %) 0))
       (first)))

;; Single point of truth -- the shortest value at the lowermost layer
;; is the "actual duration". It is scaled by all tuplets above
;; it. Event-durations are "written" durations.

(defn is-active-node?
  [node]
  (and (map? node)
       (contains? node :event)))

(defn get-duration-of-node
  [node]
  (let [[num denom] (:w-duration node)]
    (/ num denom)))

(defn get-node-dur!
  [dur-atom node]
  (if (is-active-node? node)
    (swap! dur-atom + (get-duration-of-node node))
    node))

(defn get-m-dur
  [measure]
  (let [dur-atom (atom 0)]
    (clojure.walk/prewalk (partial get-node-dur! dur-atom)
                          measure)
    @dur-atom))

(defn get-next-measure
  [measure-seq dur coll]
  (let [next-measure-dur (get-m-dur (first measure-seq))]
    (cond (>= next-measure-dur dur)
          (conj coll (first measure-seq))
          :else
          (get-next-measure (rest measure-seq)
                            (- dur next-measure-dur)
                            (conj coll (first measure-seq))))))

(defn get-measures [measure-seq durations]
  (let [total-dur (reduce + 0 durations)]
    (get-next-measure (flatten (cycle [measure-seq]))
                      total-dur
                      [])))

(defn decrement-duration
  [vertical-moment]
  (map (fn [x] (update-in x [:duration] (fn [y] (- y 1/8))))
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

(defn set-event!
  [events-atom form]
  (if (and (map? form)
           (contains? form :event))
    (do (swap! events-atom update-events)
        (assoc form :events (current-event @events-atom)))
    form))

(defn insert-events [measure events]
  (let [d (atom events)]
    (clojure.walk/prewalk (partial set-event! d)
                          measure)))

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
  (let [pitches (non-empty-pitchsets node)]
    (and (every? #(= % (first pitches)) (rest pitches))
         (not (nil? (first pitches)))
         (not (empty? pitches))
         (> (count pitches) 1))))

(defn can-update?
  [form]
  (and (not (nil? (:children form)))
       (all-children-same-pitch? form)
       (not (contains? form :top-level))))

(defn update-child
  [form]
  (if (can-update? form)
    (assoc form :events
           ((comp :events first :children) form))
    form))

(defn update-children
  [measure]
  (clojure.walk/postwalk update-child measure))

(defn is-rest?
  [form]
  (and (map? form)
       (contains? form :w-duration)
       (nil? (:events form))
       (nil? (:children form))))

(defn maybe-insert-rest
  [form]
  (if (is-rest? form)
    (assoc form :events [{:pitch "rest"}])
    form))

(defn insert-rests [measure]
  (clojure.walk/prewalk maybe-insert-rest measure))

(defn get-durations
  [vertical-moments]
  (map (comp :duration get-melodic-event) vertical-moments))

(defn concat-measure-trees
  [durations measures]
  {:duration :?
   :top-level true
   :children (flatten
              (get-measures measures
                            durations))})

(defn make-r-tree
  [events time-signatures]
  (let [root (concat-measure-trees (get-durations events)
                                   time-signatures)]
  (->> (insert-events root events)
       ((fn [x] {:children (:children x)})))))

;; TODO: join adjacent tuplets where all notes have the same ids.
;; TODO: convert tripleted two-note groups with equal length to eigth notes.
;; TODO: clojure.zip
;; TODO: attach time signatures.
;; TODO: filter parts.
;; TODO: insert FixedDurationTuplet when needed.

;; TODO: test with real events. Skip onsets?

;;-------------------------------------------------------------------
;; Convenience functions for creating rhythmic trees.
