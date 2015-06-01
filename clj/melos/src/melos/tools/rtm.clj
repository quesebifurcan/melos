(ns melos.tools.rtm
  (:require [melos.tools.make-note :refer [make-note]]
            [schema.core :as s]
            [melos.tools.schemata :as ms]
            ;; [melos.tools.measures :refer [measure-1
            ;;                               measure-2]]))
            ))

(s/defn get-melodic-event
  :- ms/Note
  [vertical-moment :- ms/VerticalMoment]
  (->> vertical-moment
       (filter #(= (:count %) 0))
       (first)))

;; Tuplets: calculate separately and apply to existing music.
;; Quantization is applied to a flat list of events.

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

(defn get-m-dur
  [measure]
  (let [dur (atom 0)]
    (clojure.walk/prewalk
     (fn [node]
       (if (is-active-node? node)
         (swap! dur + (get-duration-of-node node))
         node))
     measure)
    @dur))

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
       (concat [new-first] (rest (rest events)))))
   :else
    (let [new-first (decrement-duration (first events))]
      (into [] (concat [new-first] (rest events))))))

(defn current-id [events]
  (if (empty? events)
    nil
    (:id (first events))))

(defn current-event [events]
  (if (empty? events)
    nil
    (first events)))

(defn set-events [measure events]
  (let [d (atom events)]
    (clojure.walk/prewalk
     #(if (and (map? %)
               (contains? % :event))
        (do (swap! d update-events)
            (let [curr-id (current-id @d)]
              ;; (assoc % :events curr-id)))
              (assoc % :events (current-event @d))))
        %)
     measure)))

(defn pitchset-of-event
  [event]
  (set (map :pitch event)))

(defn all-children-same-pitch?
  [node]
  (let [children (:children node)
        events (map :events children)
        pitches (filter #(not (empty? %))
                        (map pitchset-of-event events))]
    (and (every? #(= % (first pitches)) (rest pitches))
         (not (nil? (first pitches)))
         (not (empty? pitches))
         (> (count pitches) 1))))

(defn update-children
  [measure]
    (clojure.walk/postwalk
     #(if (and (not (nil? (:children %)))
               (all-children-same-pitch? %)
               (not (contains? % :top-level)))
        (-> %
            (assoc :events (:events (first (:children %)))))
            %)
     measure))

(defn insert-rests [measure]
    (clojure.walk/prewalk
     #(if (and (map? %)
               (contains? % :w-duration)
               (nil? (:events %))
               (nil? (:children %)))
            (assoc % :events [{:pitch "rest"}])
        %)
     measure))

(defn get-durations
  [vertical-moments]
  (map (comp :duration get-melodic-event) vertical-moments))

(defn top-measure
  [durations measures]
  {:duration :?
   :top-level true
   :children (into []
                   (flatten
                    (get-measures measures
                                  durations)))})

(require '[melos.tools.utils :refer [export-to-json]])

(defn calculate-result
  [events time-signatures]
  (->> (set-events (top-measure (get-durations events)
                                time-signatures)
                   events)
       (:children)
       ((fn [x] {:children x}))
       ))

;; TODO: join adjacent tuplets where all notes have the same ids.
;; TODO: convert tripleted two-note groups with equal length to eigth notes.
;; TODO: clojure.zip
;; TODO: attach time signatures.
;; TODO: filter parts.
;; TODO: insert FixedDurationTuplet when needed.

;; TODO: test with real events. Skip onsets?

;;-------------------------------------------------------------------
;; Convenience functions for creating rhythmic trees.

