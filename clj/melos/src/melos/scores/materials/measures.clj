(ns melos.scores.materials.measures)

(require '[melos.utils :refer [ratio->non-reduced-ratio-vector]])

(defn ratio-calc
  [f args]
  (apply f (map (fn [[num denom]] (/ num denom)) args)))

(def rv+ (partial ratio-calc +))
(def rv- (partial ratio-calc -))

(defn get-child-durations
  [children]
  (->> (map first children)
       (rv+)
       (ratio->non-reduced-ratio-vector)))

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

(def measure-2-4
  [[2 4]
   [[[1 4]
     [[[1 8]] [[1 8]]]]
    [[1 4]
     [[[1 8]] [[1 8]]]]]])

(def measure-3
  (let [measure
        [[4 4]
         [[[3 4]
           [measure-2-4
            measure-2-4]]
          measure-2-4]]]
    (parse-rtm-tree-node measure)))

(def measure-4
  (let [measure
        [[4 4]
         [measure-2-4
          measure-2-4]]]
    (parse-rtm-tree-node measure)))
