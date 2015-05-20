(ns melos.tools.filter-parts
  (:require [melos.tools.rtm :as rtm]))

(defn part-events-in-node
  [part-name node]
  (let [events (:events node)
        part-events (filter #(= (:part %) part-name) events)]
    (if (empty? part-events)
      [{:pitch "rest" :part part-name}]
      part-events)))

(defn filter-by-part-names
  [part-names parts-tree]
    (clojure.walk/prewalk
     #(if (and (map? %)
               (contains? % :events))
        (-> %
            (assoc :events (part-events-in-node part-names %))
            (dissoc :event))
        %)
     parts-tree))

(defn split-out-part
  [tree part-name]
  (let [all-parts (rtm/calculate-result tree)]
    (->> (filter-by-part-names part-name all-parts))))
