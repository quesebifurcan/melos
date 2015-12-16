(ns melos.score.group-c
  (:require [clojure.math.combinatorics :as combinatorics]
            [melos.lib
             [note :refer [make-note]]
             [chord :as chord]
             [utils :as utils]]
            [melos.score
             [combinations :refer [unfold-parameters]]
             [score-utils :as score-utils]]
            [melos.score.materials
             [measures :as measures]]))

(defn transpose-all
  [step forms]
  (clojure.walk/postwalk
   (fn [form]
     (cond (number? form)
           (+ form step)
           :else
           form))
   forms))

(defn upper
  []
  (letfn [(blueprint [{:keys [pitches
                              part-name
                              transposition
                              dur
                              drop-n]}]
            {:pitch (->>
                     (range 13)
                     (map (fn [x] [x]))
                     (transpose-all transposition))
             :part [part-name]
             :fn utils/make-chord-from-pitch-vector-params
             :partition (partial utils/cyclic-partition [1 2 1 2 3])
             :merge-left? [false]
             ;; :dissonance-contributor? [false false true]
             :duration dur
             :merge-right? [false]
             :drop-n drop-n})]
    (->> {:part-name [:upper]
          :pitches [[0 2 4 5 7 -3 -2 3 1 6 5]]
          :transposition [0 12]
          :drop-n (range 5)
          :dur [[1/4]]}
         (unfold-parameters)
         (map (comp utils/unfold-events blueprint)))))

(defn lower
  []
  (letfn [(blueprint [{:keys [pitches
                              part-name
                              transposition
                              drop-n
                              dur]}]
            {:pitch (->>
                     (range 7)
                     (map (fn [x] [x]))
                     (transpose-all transposition))
             :part [part-name]
             :fn utils/make-chord-from-pitch-vector-params
             :partition (partial utils/cyclic-partition [1 1 1])
             :drop-n drop-n
             :duration dur})]
    (->> {:part-name [:lower]
          :pitches [[0 2 4 5 7 9 10 3 1 6 5]]
          :transposition [-4 8]
          :drop-n [0 1 2 3 4]
          :dur [[1/4]]}
         (unfold-parameters)
         (map (comp utils/unfold-events blueprint)))))

(defn ped
  []
  (letfn [(blueprint [{:keys [pitches
                              part-name
                              transposition
                              drop-n
                              dur]}]
            {:pitch (->> [[0] [0 2] [2] [2 3] [3]]
                         (transpose-all transposition))
             :part [part-name]
             :fn utils/make-chord-from-pitch-vector-params
             :partition (partial utils/cyclic-partition [1])
             :drop-n drop-n
             :duration dur})]
    (->> {:part-name [:ped]
          :pitches [:not-used]
          :transposition [-20]
          :drop-n (range 2)
          :dur [[1/4]]}
         (unfold-parameters)
         (map (comp utils/unfold-events blueprint)))))

(def materials
  {:upper (upper)
   :lower (lower)
   :ped (ped)
   :melodic-indices [(take 18 (cycle [:upper :lower :ped]))
                     (take 13 (cycle [:upper :lower :ped]))]})

(def partition-events-fn
  (partial score-utils/part-count-sufficient? 3))

(defn filter-events-fn
  [events]
  (and (>= (count events) 6)
       (every? (partial score-utils/part-count-sufficient? 3) events)))

(def diss-params
  {:check (fn [events]
            (<= (chord/scaled-dissonance-value (map :pitch events))
                (chord/scaled-dissonance-value [0 1 2])))})

(def initial-state
  {:diss-fn-params {:max-count 100
                    :max-lingering 300
                    ;; TODO: pass on diss-value
                    :diss-params diss-params}
   :tempo 200
   ;; TODO: measures is not used -- it is hardcoded in compose-score/compose
   :measures [measures/measure-3]
   :pre []
   :post []
   :part-names [:upper :lower :ped]})

(defn get-average-dissonance
  [phrase]
  (let [pitch-sets (map #(map :pitch %) phrase)
        durations (map chord/get-melodic-duration phrase)
        dissonances (map (fn [ps dur]
                           (* (chord/scaled-dissonance-value ps)
                              dur))
                         pitch-sets
                         durations)]
    (/ (apply + dissonances) (count pitch-sets))))

;; (defn get-average-pitch
;;   [phrase]
;;   (let [pitches (mapcat #(map :pitch %) phrase)]
;;     (/ (apply + pitches) (count pitches))))

(defn get-max-dissonance
  [phrase]
  (let [pitch-sets (map #(map :pitch %) phrase)
        durations (map chord/get-melodic-duration phrase)
        dissonances (map chord/scaled-dissonance-value pitch-sets)]
    (apply max dissonances)))

(defn post-process
  [events]
  (->> events
       (utils/distinct-by score-utils/pitch-profile)
       (sort-by get-average-dissonance)
       (take 30)
       (partition 1)
       (assoc {} :segments)))

(def session-config
  {:persist-to "testing-c.edn"
   :params {:chord-seqs materials
            :initial-state-fn initial-state
            :post-process post-process}})

