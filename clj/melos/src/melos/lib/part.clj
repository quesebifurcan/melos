(ns melos.lib.part
  (:require [melos.lib.rhythm-tree :as rtm]))

(defn maybe-insert-rest
  [x]
  (if (:is-rest? x)
    (assoc x :pitch "rest" :part (:part-name x))
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

(defn compose-part
  [tempo part-names rtm-tree]
  {:tempo tempo
   :parts (->> (map
                (fn [part-name]
                  {:part-name part-name
                   :events (split-out-part rtm-tree part-name)})
                part-names)
               ;; Comment this line to get a "pulsed" version of
               ;; the music; no notes will be merged.
               ;; TODO: each gesture specifies how it should make
               ;; use of this.
               (rtm/merge-all-tied)
               )})
