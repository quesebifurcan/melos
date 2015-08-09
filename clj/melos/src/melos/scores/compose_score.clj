(ns melos.scores.compose-score
  (:require [clojure.math.combinatorics :as combinatorics]
            [melos.schemas.schemas :as ms]
            [melos.scores.materials
             [dissonance-maps :as dissonance-maps]
             [event-seqs :as event-seqs]
             [measures :as measures]
             [pairwise-mod :as pairwise]
             [stepwise-mod :as stepwise]]
            [melos.segment.graphs :as graphs]
            [melos.utils
             [cycle-params :refer [unfold-parameter-cycles]]
             [utils :as utils]]
            [schema.core :as s]))

;; TODO: move?

(defn update-score-state
  [initial-state updates]
  (->> (utils/update-state initial-state updates)
       (utils/evaluate-nested-fns)))

(defn unfold-parameters
  [m]
  (map (fn [x] (zipmap (keys m) x))
       (apply combinatorics/cartesian-product (vals m))))

(defn upper-part
  [{:keys [transposition part-name]}]
  (utils/unfold-events (event-seqs/upper part-name transposition)))

(defn compose-all
  [filters phrases]
  (->> (filters phrases)
       (map graphs/compose-segment)))

(defn compose
  []
  (let [changes (unfold-parameters
                 {[:melody-sources :upper/a :params :transposition] [-10 10 0]
                  [:melodic-indices] [
                                      {:fn (fn [x] (take 21 (cycle x)))
                                       :params [:upper/a :lower/a :ped/a]}
                                      {:fn (fn [x] (take 10 (cycle x)))
                                       :params [:upper/a :lower/a :upper/a :ped/a]}
                                      ]
                  [:diss-fn-params :diss-value] [[0 2 4 5] [0 1 2]]
                  })
        state {:melody-sources {
                                :upper/a {:fn upper-part
                                          :params {:transposition -3
                                                   :part-name :upper}}
                                :lower/a {:fn upper-part
                                          :params {:transposition -5
                                                   :part-name :lower}}
                                :ped/a {:fn upper-part
                                        :params {:transposition -15
                                                 :part-name :ped}}
                                }
               :diss-fn-params {:max-count 10
                                :max-lingering 300
                                :diss-value [0 2 4 5]}
               :interval->diss-map dissonance-maps/default
               :time-signatures [measures/measure-4]
               :measures [measures/measure-4]
               :mod-dur-patterns []
               :tempo 240
               :last-event-extension 7/4
               :part-names [:upper :lower :ped]
               :melodic-indices []}
        filters #(sort-by (fn [x]
                            (apply + (map count (:merged-chord-seq x))))
                          %)]
    (->> (map (fn [change-set] (update-score-state state change-set))
              changes)
         (compose-all filters))))

;; (into {}
;;       (compose))

;; (into {}
;;       (:result
;;        (first
;;         (compose))))

         ;; (map compose-segment/compose-event-seq)
         ;; (compose-all filters))))


;; main
;; (compose event-seq rtm-data)
;; (compose event-seq rtm-data)
