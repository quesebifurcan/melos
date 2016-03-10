(ns melos.lib.measure
  (:require [melos.lib.utils :as utils]))

(defn ratio-calc
  [f args]
  (apply f (map (fn [[num denom]] (/ num denom)) args)))

(def rv+ (partial ratio-calc +))
(def rv- (partial ratio-calc -))

(defn get-child-durations
  [children]
  (->> (map first children)
       (rv+)
       (utils/ratio-to-non-reduced-ratio-vector)))

(defn parse-rtm-tree-node
  [[dur children]]
  (if ((complement nil?) children)
    (let [w-dur (get-child-durations children)]
      {:duration dur
       :w-duration w-dur
       :children (map parse-rtm-tree-node children)})
    {:duration dur
     :w-duration dur
     :children nil
     :event nil}))

(defn stretch-tree
  [duration curr-prolation child-prolations]
  (let [divs {[7 4] [[4 4] [3 4]]
              [6 4] [[2 4] [2 4] [2 4]]
              [5 4] [[3 4] [2 4]]
              [4 4] [[2 4] [2 4]]
              [3 4] [[2 4] [2 8]]
              [2 4] [[2 8] [2 8]]
              [3 8] [[2 8] [1 8]]
              [2 8] [[1 8] [1 8]]}]
    (if (and (seq child-prolations)
             (contains? divs duration))
      (let [curr-timeframe (update-in duration [0] (fn [x] (+ curr-prolation x)))
            branch-durations (get divs curr-timeframe)]
        (conj [duration]
              (mapv (fn [dur prolation] (stretch-tree dur prolation (rest child-prolations)))
                    branch-durations
                    (cycle (first child-prolations)))))
      [duration])))
