(ns melos.tools.filter-parts
  (:require [melos.tools.rhythm-tree.rtm :as rtm]))

(defn maybe-insert-rest
  [x]
  (if (:is-rest? x)
      {:pitch "rest" :part (:part-name x)}
      x))

(defn part-events-in-node
  [part-name node]
  (let [events (:events node)
        part-events (filter #(= (:part %) part-name) events)]
    (if (empty? part-events)
      [{:pitch "rest" :part part-name}]
      (map maybe-insert-rest part-events))))

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
  (filter-by-part-names part-name tree))
