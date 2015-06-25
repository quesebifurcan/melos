(ns melos.scores.utils
  (:require [melos.tools.rtm :as rtm]
            [melos.tools.make-note :refer [make-note]]
            [melos.tools.utils :refer [merge-in rotate]]))


(defn maybe-vec [x] (if (number? x) [x] x))

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

(defn cyclic-partition
  [splits xs]
  (cons (take (first splits) xs)
        (lazy-seq (cyclic-partition (rotate splits)
                                    (drop (first splits) xs)))))

(defn cyclic-repeats
  [repeats xs]
  (if (seq xs)
    (concat (repeat (first repeats) (first xs))
            (lazy-seq (cyclic-repeats (rotate repeats)
                                      (rotate xs))))))

(defn combine-partitions
  [& partitions]
  (let [cycle-len (apply * (map (fn [x] (apply + x))
                                partitions))
        seqs (map (fn [part]
                    (take-while (fn [x] (< x cycle-len))
                                (reductions + 0 (cycle part))))
                  partitions)]
    (->> (sort (set (apply concat seqs)))
         (partition 2 1)
         (map (fn [[x y]] (- y x))))))
