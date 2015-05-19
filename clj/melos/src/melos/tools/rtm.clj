(ns melos.tools.rtm
  (require [melos.tools.make-note :refer [make-note]]))

(def durations [1/4 1/16 1/16 1/16 3/4 1/8 1/16 1/4])

(def onsets
  (reductions + 0 (take 16 (cycle [1/16]))))

(defn quantize-dur
  [onset dur]
  (->> (filter #(>= % (+ onset dur)) onsets)
       (map #(rem % dur))
       ))

(defn get-melodic-event
  "Retrieve the most recently added event in *vertical-moment*."
  [vertical-moment]
  (first (filter #(= (:count %) 0) vertical-moment)))

(quantize-dur 1/16 3/4)

;; Tuplets: calculate separately and apply to existing music.
;; Quantization is applied to a flat list of events.

;; Single point of truth -- the shortest value at the lowermost layer
;; is the "actual duration". It is scaled by all tuplets above
;; it. Event-durations are "written" durations.

(def measure-1
  {:w-duration [5 4]
   ;; :top-level true
   :duration [4 4]
   :children [
              {:w-duration [4 4]
               :duration [3 4]
               :children [
                          {:w-duration [2 4]
                           :duration [2 4]
                           :children [
                                      {:w-duration [1 4]
                                       :duration [1 4]
                                       :event nil
                                       :children nil}
                                      {:w-duration [1 4]
                                       :duration [1 4]
                                       :event nil
                                       :children nil}
                                      ]}
                          {:w-duration [2 4]
                           :duration [2 4]
                           :children [
                                      {:w-duration [1 4]
                                       :duration [1 4]
                                       :event nil
                                       :children nil}
                                      {:w-duration [1 4]
                                       :duration [1 4]
                                       :event nil
                                       :children nil}
                                      ]}
                          ]}
              {:w-duration [2 4]
               :duration [2 4]
               :children [
                          {:w-duration [1 4]
                           :duration [1 4]
                           :event nil
                           :children nil}
                          {:w-duration [1 4]
                           :duration [1 4]
                           :event nil
                           :children nil}
                          ]}
              ]})

(def measure-2
  {:w-duration [5 4]
   :duration [4 4]
   :children [
              {:w-duration [2 4]
               :duration [2 4]
               :children [
                          {:w-duration [1 4]
                           :duration [1 4]
                           :event nil
                           :children nil}
                          {:w-duration [1 4]
                           :duration [1 4]
                           :event nil
                           :children nil}
                          ]}
              {:w-duration [2 4]
               :duration [2 4]
               :children [
                          {:w-duration [1 4]
                           :duration [1 4]
                           :event nil
                           :children nil}
                          {:w-duration [1 4]
                           :duration [1 4]
                           :event nil
                           :children nil}
                          ]}
              ]})
  
(defn get-m-dur [m]
  (let [dur (atom 0)]
    (clojure.walk/prewalk
     #(if (and (map? %)
               (contains? % :event))
        (swap! dur + (/ (first (:w-duration %))
                        (second (:w-duration %))))
        %)
     m)
    @dur))

(defn get-next-measure
  [measure-seq dur coll]
  ;; (let [events-dur (reduce + (map :duration events))]
  ;;   (get-m-dur measure)))
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
  (map (fn [x] (update-in x [:duration] (fn [y] (- y 1/4))))
       vertical-moment))

(defn update-events
  [events]
  ;; TODO: specify which duration to subtract and check that no event
  ;; is shorter than this duration.
  (cond
   (empty? events)
   nil

   (< (:duration (get-melodic-event (first events))) 1/4)
   (if (empty? (rest events))
     nil
     (let [new-first (first (rest events))
           new-first (decrement-duration new-first)]
       (concat [new-first] (rest (rest events)))))
    ;; (update-in (into [] (rest events))
    ;;            [0]
    ;;            (fn [y]
    ;;              (map (fn [x] (update-in x [:duration] #(- % 1/4)))))))

     ;; (update-in (into [] (rest events))
     ;;            [0 0 :duration]
     ;;            (fn [x] (- x 1/4))))

   :else
    (let [new-first (decrement-duration (first events))]
      (into [] (concat [new-first] (rest events))))))
   ;; (update-in events
   ;;            [0]
   ;;            (fn [y]
   ;;              (map (fn [x] (update-in x [:duration] #(- % 1/4))))))))

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

(defn all-children-same-pitch?
  [node]
  (let [children (:children node)
        events (mapcat :events children)
        pitches (map :pitch events)
    pitches (filter #(not (empty? %))
                     (map (fn [x] (map :pitch x))
                          (map :events children)))]
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
            (assoc :events (:events (first (:children %))))
            ;; (assoc :children nil)
            )
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

(def events
  (let [pitches (range -10 30)
        durations (cycle [17/4 3/4 1/4 1/4 6/4 3/4])]
    (into [] (map (fn [x y] [(make-note :pitch x :duration y)
                             (make-note :pitch 0 :duration y)
                             ])
                  pitches
                  durations))))

(defn get-durations
  [vertical-moments]
  (map (comp :duration get-melodic-event) vertical-moments))

(defn top-measure
  [durations]
  {:duration :?
   :top-level true
   :children (into []
                   (flatten
                    (get-measures [measure-1 measure-2]
                                  durations)))})

(defn print-durations
  [measure]
    (clojure.walk/prewalk
     #(if (not (nil? (:events %)))
        (do (println (:duration %))
            %)
        %)
     measure))

(require '[melos.tools.utils :refer [export-to-json]])

(defn calculate-result
  [events]
  (->> (set-events (top-measure (get-durations events))
                   events)
       (:children)
       ;; (update-children)
       ;; (update-children)
       ;; (update-children)
       ;; (update-children)
       ;; (update-children)
       ;; (insert-rests)
       ;; (update-children)
       ;; (update-children)
       ;; (update-children)
       ;; (update-children)
       ;; (update-children)
       ((fn [x] {:children x}))
       
       ;; ;; (update-children)
       ;; ;; (update-children)
       ;; ;; (update-children)
       ;; ;; (update-children)
       ;; ;; (update-children)
       ;; ;; (update-children)
       ;; ;; (update-children)
       ;; ;; (update-children)
       ;; ;; (print-durations)
       ;; ;; (update-children)
       ;; ;; (update-children)
       ;; ;; (update-children)
       ;; ;; (insert-rests)
       ;; ;; (update-children)
       ;; ;; (update-children)
       ))

;; (export-to-json "/Users/fred/Desktop/time-signatures.json"
;;                 (calculate-result events))

;; (println "\n")

;; (calculate-result events)

;; result

;; (->> result
;;      (update-children)
;;      (update-children-2)
;;      (:children)
;;      (first)
;;      )

;; result

;; TODO: join adjacent tuplets where all notes have the same ids.
;; TODO: convert tripleted two-note groups with equal length to eigth notes.
;; TODO: clojure.zip
;; TODO: attach time signatures.
;; TODO: filter parts.
;; TODO: insert FixedDurationTuplet when needed.

;; TODO: test with real events. Skip onsets?

