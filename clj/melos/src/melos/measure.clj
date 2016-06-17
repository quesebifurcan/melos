(ns melos.measure
  (:require [clojure.zip :as zip]
            [melos.utils :as utils])
  (:import melos.schemas.Chord))

(defn ratio-calc
  [f args]
  (apply f (map (fn [[num denom]] (/ num denom)) args)))

(def rv+ (partial ratio-calc +))
(def rv- (partial ratio-calc -))

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
     :type :RhythmTreeNode
     :children (map (partial make-rtm-tree template) nxt)}))

(defn rtm-tree-zipper
  [m]
  (zip/zipper :children :children (fn [node children] (assoc node :children children)) m))

(defn get-loc-duration
  [loc]
  (let [node (zip/node loc)]
    (if ((comp zero? :sum-of-leaves-duration) node)
      (:duration node)
      (:sum-of-leaves-duration node))))

(defn insert-chords [notes loc]
  (if (zip/end? loc)
    (zip/node loc)
    (if (map? (zip/node loc))
      (cond (empty? notes)
            (recur []
                   (zip/next (zip/edit loc #(assoc % :chord {:rest true} :children []))))
            (:root (zip/node loc))
            (recur notes (zip/next loc))
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
                   (zip/next loc)))
      (recur notes
             (zip/next loc)))))
