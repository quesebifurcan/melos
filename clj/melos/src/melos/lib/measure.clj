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

(defn parse-rtm-tree-node*
  [[dur children]]
  (if ((complement nil?) children)
    (let [w-dur (get-child-durations children)]
      {:duration dur
       :written-duration w-dur
       :event nil
       :children (map parse-rtm-tree-node* children)})
    {:duration dur
     :written-duration dur
     :event nil
     :children nil}))

(defn parse-rtm-tree-node
  [[dur children]]
  (parse-rtm-tree-node* [dur children]))

(defn stretch-tree
  [duration curr-prolation child-prolations]
  (let [divs {[7 4] [[4 4] [3 4]]
              [6 4] [[2 4] [2 4] [2 4]]
              [5 4] [[3 4] [2 4]]
              [4 4] [[2 4] [2 4]]
              [3 4] [[2 4] [2 8]]
              [2 4] [[2 8] [2 8]]
              [3 8] [[2 8] [1 8]]
              [2 8] [[1 8] [1 8]]
              [1 8] [[1 16] [1 16]]
              [1 16] [[1 32] [1 32]]
              }]
    (if (and (seq child-prolations)
             (contains? divs duration))
      (let [curr-timeframe (update-in duration [0] (fn [x] (+ curr-prolation x)))
            branch-durations (get divs curr-timeframe)]
        (conj [duration]
              (mapv (fn [dur prolation] (stretch-tree dur prolation (rest child-prolations)))
                    branch-durations
                    (cycle (first child-prolations)))))
      [duration])))

(stretch-tree [4 4] 1 [[0] [0] [0] [0]])
;; =>
;; [[4 4]
;;  [[[3 4]
;;    [[[2 4]
;;      [[[2 8]
;;        [[[1 8]]
;;         [[1 8]]]]
;;       [[2 8]
;;        [[[1 8]]
;;         [[1 8]]]]]]
;;     [[2 8]
;;      [[[1 8]]
;;       [[1 8]]]]]]
;;   [[2 4]
;;    [[[2 8]
;;      [[[1 8]]
;;       [[1 8]]]]
;;     [[2 8]
;;      [[[1 8]]
;;       [[1 8]]]]]]]]

(stretch-tree [4 4] 1 [[1 0] [1] [0] [0]])
;; =>
;; [[4 4]
;;  [[[3 4]
;;    [[[2 4]
;;      [[[2 4]
;;        [[[2 8]]
;;         [[2 8]]]]
;;       [[2 8]
;;        [[[1 8]]
;;         [[1 8]]]]]]
;;     [[2 4]
;;      [[[2 4]
;;        [[[2 8]]
;;         [[2 8]]]]
;;       [[2 8]
;;        [[[1 8]]
;;         [[1 8]]]]]]]]
;;   [[2 4]
;;    [[[2 8]
;;      [[[2 8]
;;        [[[1 8]]
;;         [[1 8]]]]
;;       [[1 8]]]]
;;     [[2 8]
;;      [[[2 8]
;;        [[[1 8]]
;;         [[1 8]]]]
;;       [[1 8]]]]]]]]

(def duration-resolutions
  {1              [[3/4 :stretch] [2/4 :stretch]]
   [3/4 :stretch] [2/4 2/4]
   2/4            [[2/4 :stretch] 1/4]
   [2/4 :stretch] [1/4 1/4 [1/4 :stretch]]
   [1/4 :stretch] [1/8 1/8 1/8]
   1/4            [1/8 1/8]})

;; (def duration-resolutions
;;   {1 [2/4 2/4]
;;    2/4 [1/4 1/4]
;;    1/4 [1/8 1/8]})

(defn get-duration [node] (if (vector? node) (first node) node))

(defn get-summed-durations
  [template node]
  (if-let [nxt (get template node)]
    (map (partial get-summed-durations template)
         nxt)
    (get-duration node)))

(defn insert-node
  [template node]
  (let [nxt (get template node)]
    {:duration (get-duration node)
     :sum-of-leaves-duration (->> (get-summed-durations template node)
                                  flatten
                                  (apply +))
     :children (map (partial insert-node template) nxt)}))

;; TODO: multiple events, sequence of measures
(let [measure (insert-node duration-resolutions 1)
      note (atom {:pitch 0 :w-duration 1/8})]
  (->>
   (clojure.walk/prewalk
    (fn [node]
      (if (and (map? node)
               (:children node))
        (cond (<= (:w-duration @note) 0)
              (assoc node :rest true :children nil)
              (empty? (:children node))
              (do (swap! note
                         #(update % :w-duration (fn [x] (- x (:duration node)))))
                  (assoc node :events (assoc @note :w-duration (:duration node))))
              (> (:sum-of-leaves-duration node)
                 (:w-duration @note))
              node
              (>= (:w-duration @note)
                  (:sum-of-leaves-duration node))
              (do (swap! note
                         #(update % :w-duration
                                  (fn [x] (- x (:sum-of-leaves-duration node)))))
                  (assoc node :events (assoc @note :w-duration (:duration node))
                         :children nil))
              :else
              nil)
        node))
    [measure measure measure])
   ))

;; -- recipe?
;; (let [a (atom 0)
;;       b (repeat 1)]
;;   (take-while (fn [x] (< @a 18))
;;               (map (fn [x] (do (swap! a inc) x))
;;                    b)))
