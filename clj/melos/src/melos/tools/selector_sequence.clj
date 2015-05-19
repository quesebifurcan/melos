(ns melos.tools.selector-sequence
  (:require [melos.tools.utils :refer [rotate]]))

(defn- update-state
  "Helper for reduce call in function
  melos.selector-sequence.next-state."
  [curr-state event]
  (event curr-state))

(defn- update-result
  "Conj val onto :result of *state*."
  [val]
  (fn [state]
    (update-in state
               [:result]
               (fn [x] (conj x val)))))

(defn- update-used-melody
  "*accessors* is used to retrieve a melodic event from :events-seqs
  of *state*. The form of *accessors* is:

      [part-name melodic-source]

  A typical input would look like this:

      [:lower :lindenmayer]

  Following the retrieval of the melodic event, rotate the melodic
  source vector.
  "
  [accessors]
  (fn [state]
    (update-in state
               (cons :events-seqs accessors)
               (fn [x] (rotate x)))))

(defn- get-new-value
  "Given a vector of *accessors*, retrieve the next melodic event from
  *state*."
  [state accessors]
  (first (get-in state (cons :events-seqs accessors))))

(defn- next-state
  "Update *state*. See update-result and update-used-melody for more details."
  [state accessors]
  (let [new-val (get-new-value state accessors)]
    ;; TODO: rewrite with threading macro.
    (reduce update-state state [(update-result new-val)
                                (update-used-melody accessors)])))

(defn- collect-one-melody-segment
  "Combine updates to *state* to collect the next events in segment."
  [state part-seq]
  (let [new-state (reduce next-state state part-seq)
        result (:result new-state)]
    (-> new-state
        (update-in [:sum] (fn [x] (conj x result)))
        (assoc :result []))))

(defn get-melodic-segment
  "Return a seq, *cnt* elements long, using *part-seq*
  to retrieve values in *events-seqs*."
  [part-seq part->event count]
  (take count (map (fn [x] [x (x part->event)])
                   (cycle part-seq))))

;; (defn collect-events
;;   "Collect melodic events according to score. The rather convoluted
;;   logic is explained by the fact that:

;;   - the score is divided into segments and
;;   - the state of the melodic sources needs to be passed from one
;;   segment to the next.

;;   In other words, the actual value of a reference to a melodic event
;;   changes depending on the state. For example, repeated use of [:lower
;;   :lindenmayer] as accessor will yield a sequence of different melodic
;;   events."
;;   [score events-seqs]
;;   (let [melodic-indices (map get-melodic-segment score)
;;         initial-state {:events-seqs events-seqs
;;                        :sum []
;;                        :result []}
;;         melos (->> melodic-indices
;;                       (reduce collect-one-melody-segment initial-state)
;;                       (:sum))]
;;     melos))

(require '[melos.tools.utils :refer [rotate]])

(defn get-and-rotate
  [state accessor]
  (let [
        v (first (get-in @state accessor))]
    (do (swap! state update-in accessor (fn [x] (drop 1 x)))
        v)))


(defn collect-events
  "Collect melodic events according to score. The rather convoluted
  logic is explained by the fact that:

  - the score is divided into segments and
  - the state of the melodic sources needs to be passed from one
  segment to the next.

  In other words, the actual value of a reference to a melodic event
  changes depending on the state. For example, repeated use of [:lower
  :lindenmayer] as accessor will yield a sequence of different melodic
  events."
  [score events-seqs]
  (let [melodic-indices (map get-melodic-segment score)]
    (map (fn [x] (map (fn [y] (get-and-rotate events-seqs y)) x))
         melodic-indices)))

(defn collect-events-in-segment
  "Collect melodic events according to score. The rather convoluted
  logic is explained by the fact that:

  - the score is divided into segments and
  - the state of the melodic sources needs to be passed from one
  segment to the next.

  In other words, the actual value of a reference to a melodic event
  changes depending on the state. For example, repeated use of [:lower
  :lindenmayer] as accessor will yield a sequence of different melodic
  events."
  [melodic-indices events-seqs]
  ;; (let [melodic-indices (get-melodic-segment segment)
  ;;       events-seqs (:melody-sources segment)
  ;;       result (map (fn [x] (get-and-rotate events-seqs x))
  ;;                   melodic-indices)]
  ;;   (assoc segment :events result)))
  (map (fn [x] (get-and-rotate events-seqs x))
       melodic-indices))
