(ns melos.scores.event-seqs.upper
  (:require [clojure.walk :as walk]
            ;; [melos.tools.dissonance-calculator :refer
            ;;  [dissonance-map-default dissonance-map-2]]
            ;; [melos.scores.materials.measures :as measures]
            ;; [melos.tools.utils :refer [export-to-json]]

            ;; [melos.tools.l-systems :refer [lindenmayer]]
            [melos.tools.make-note :refer [make-note]]
            ;; [melos.tools.modify-durations :as mod-dur]
            ;; [melos.tools.rtm :refer [make-r-tree merge-all-tied]]
            ;; [melos.tools.contour :refer
            ;;  [apply-contour-to-melody]]
            ;; [melos.scores.materials.measures :refer [measure-3]]
            [melos.tools.utils :refer [rotate]]
            ;; [melos.tools.filter-parts :refer [split-out-part]]
            ;; [melos.tools.utils :refer [export-to-json]]))
            ))

(defn make-chord-from-pitch-vector-params
  [{:keys [pitch] :as m}]
  (let [group (gensym "G__")]
  (map (fn [p]
         (make-note (merge {:pitch p :group group}
                           (dissoc m :pitch))))
       pitch)))

(defn parse-params
  [x]
  (if (list? x)
    (repeat (last x) (first x))
    [x]))

(defn unfold-events
  [m part]
  (let [m (assoc m :part [part])
        f (:fn m)
        partition-fn (:partition m)
        m (dissoc m :fn :partition)]
  (->> (map (fn [x] (mapcat parse-params x))
            (vals m))
       (map cycle)
       (apply map vector)
       (map (fn [x] (zipmap (keys m) x)))
       (map f)
       (partition-fn))))

(defn cyclic-partition
  [xs splits]
  (cons (take (first splits) xs)
        (lazy-seq (cyclic-partition (drop (first splits) xs)
                                    (rotate splits)))))

(defn cyclic-repeats
  [xs repeats]
  (if (seq xs)
    (concat (repeat (first repeats) (first xs))
            (lazy-seq (cyclic-repeats (rotate xs)
                                      (rotate repeats))))))

(defn upper-soft
  []
  {:pitch
   (map-indexed (fn [i x]
                  (if (= (rem i 9) 0)
                    [x] [x]))
                (concat
                 (range -3 10)
                 (range 10 -3 -1)))
   :dissonance-contributor? [true]
   :part [:upper]
   :fn make-chord-from-pitch-vector-params
   :partition #(cyclic-partition % [1 1 1 2 2 1 1 2])
   :duration [1/4 1/4 1/4 1/4]})

