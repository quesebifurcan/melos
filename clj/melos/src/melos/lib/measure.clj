(ns melos.lib.measure
  (:require [melos.lib.utils :as utils]
            [clojure.zip :as zip])
  (:import [melos.lib.schemas Chord]))

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

(defn make-rtm-tree
  [template node]
  (let [nxt (get template node)]
    {:duration (get-duration node)
     :chord nil
     :sum-of-leaves-duration (->> (get-summed-durations template node)
                                  flatten
                                  (apply +))
     :children (map (partial make-rtm-tree template) nxt)}))

;; (defn insert-notes
;;   [notes measures]
;;   ;; TODO: multiple events, sequence of measures
;;   (let [measure (insert-node duration-resolutions 1)
;;         note (atom {:pitch 0 :w-duration 5/8})]
;;     (->> (clojure.walk/prewalk
;;           (fn [node]
;;             (if (and (map? node)
;;                      (:children node))
;;               (cond (<= (:w-duration @note) 0)
;;                     (assoc node :rest true :children nil)
;;                     (empty? (:children node))
;;                     (do (swap! note
;;                                #(update % :w-duration (fn [x] (- x (:duration node)))))
;;                         (assoc node :events (assoc @note :w-duration (:duration node))))
;;                     (> (:sum-of-leaves-duration node)
;;                        (:w-duration @note))
;;                     node
;;                     (>= (:w-duration @note)
;;                         (:sum-of-leaves-duration node))
;;                     (do (swap! note
;;                                #(update % :w-duration
;;                                         (fn [x] (- x (:sum-of-leaves-duration node)))))
;;                         (assoc node :events (assoc @note :w-duration (:duration node))
;;                                :children nil))
;;                     :else
;;                     nil)
;;               node))
;;           [measure measure]))))

(defn maybe-insert-chord
  [node chord]
  chord)

(defn rtm-tree-zipper
  [m]
  (zip/zipper :children :children (fn [node children] (assoc node :children children)) m))

(defn maybe-edit
  [num loc]
  (zip/edit loc (fn [x] (if (map? x) (assoc x :oij num) x))))

(defn fast-forward-z [nums loc]
  (if (zip/end? loc)
    loc
    (cond (and (map? (zip/node loc))
               (not (= (first nums) 0))
               (= 0 (rem (first nums) 2)))
          (recur (rest nums)
                 (zip/next (zip/edit loc (fn [x] (-> x (assoc :oij (first nums)) (assoc :children []))))))
          (map? (zip/node loc))
          (recur (rest nums)
                 (zip/next (zip/edit loc #(assoc % :oij (first nums)))))
          :else
          (recur nums
                 (zip/next loc)))))

;; (defn insert-events
;;   [loc events]
;;   (if (zip/end? loc)
;;     loc
;;     (let [head (first events)]
;;       (cond (not head)

(defn insert-events
  [loc events]
  (assoc (zip/node loc) :chord events))

;; TODO:
;; (defn insert-chord
;;   [node chord]
;;   (let [node-duration (:duration (zip/node loc))
;;         chord-duration (:duration chord)]
;;     (cond (>= chord-duration node-duration)
;;           ;; assoc chord onto node, set :children of node to nil.
;;           ;; return: [edited_loc (chord - node_duration)]
;;           nil
;;           (< chord-duration node-duration)
;;           ;; return [loc chord] so that parent function can call next
;;           nil
;;           )))

;; return pairs? loc + events?

;; (defn insert-event
;;   [loc event]
;;   (let [loc-duration (:duration loc)
;;         event-duration (:duration event)]
;;   (cond (>= event-duration loc-duration)
;;         (insert-event

(defn fast-forward-z [nums loc]
  (if (zip/end? loc)
    loc
    (cond (map? (zip/node loc))
          (recur (rest nums)
                 (zip/next (zip/edit loc (fn [x] (assoc x :oijoij (first nums))))))
          :else
          (recur nums
                 (zip/next loc)))))

(-> {:children [
                {:a 1 :children [{:a 2 :children [{:a 3 :children nil}
                                                  {:a 4 :children nil}]}
                                 {:a 5 :children [{:a 6 :children nil}]}]}
                {:a 8 :children [{:a 4 :children [{:a 3 :children nil}
                                                  {:a 4 :children nil}]}
                                 {:a 5 :children [{:a 6 :children nil}]}]}
                ]}
    ;; zipper
    ;; (zip/edit #(assoc % :b 987))
    ;; (zip/edit #(assoc % :children []))
    (rtm-tree-zipper)
    ((fn [x] (fast-forward-z (range) x)))
    ;; ;; (maybe-edit 123)
    ;; zip/next
    ;; zip/next
    ;; zip/next
    ;; (zip/replace 987123987123)
    ;; zip/root
    ;; ;; (maybe-edit 123)
    )

;; TODO: split into parts _before_ inserting into rhythmic tree.
;; That way, tied notes can be merged _before_ insertion.

(defn get-loc-duration
  [loc]
  (let [node (zip/node loc)]
    (if ((comp zero? :sum-of-leaves-duration) node)
      (:duration node)
      (:sum-of-leaves-duration node))))

(defn insert-chords [notes loc]
  (if (zip/end? loc)
    loc
    (if (map? (zip/node loc))
      ;; TODO: > / < / = separately
      (cond (empty? notes)
            (recur []
                   (zip/next (zip/edit loc #(assoc %
                                                   :chord {:rest true}
                                                   :children []

                                                   ))))
            (> (:duration (first notes))
               (get-loc-duration loc))
            (recur (cons (update
                          (first notes)
                          :duration (fn [x] (- x (get-loc-duration loc))))
                         (rest notes))
                   (zip/next (zip/edit loc (fn [x]
                                             (-> x
                                                 (assoc :chord (first notes))
                                                 (assoc :children []))))))
            (= (:duration (first notes))
               (get-loc-duration loc))
            (recur (rest notes)
                   (zip/next (zip/edit loc (fn [x]
                                             (-> x
                                                 (assoc :chord (first notes))
                                                 (assoc :children []))))))

            (< (:duration (first notes))
               (get-loc-duration loc))
            (recur notes
                   (zip/next loc))
            )
      (recur notes
             (zip/next loc)))))

(def duration-resolutions
  {1 [2/4 2/4]
   2/4 [1/4 1/4]
   1/4 [1/8 1/8]})

;; TODO: tests
(let [tree (rtm-tree-zipper (make-rtm-tree duration-resolutions 1))]
  (insert-chords [{:duration 3/4 :events #{{:pitch 2} {:pitch 4}}}]
                         ;; {:duration 1/8 :events #{{:pitch 3} {:pitch 19}}}
                         ;; {:duration 1/8 :events #{{:pitch 5} {:pitch 10}}}
                 tree))


;; -- recipe?
;; (let [a (atom 0)
;;       b (repeat 1)]
;;   (take-while (fn [x] (< @a 18))
;;               (map (fn [x] (do (swap! a inc) x))
;;                    b)))
