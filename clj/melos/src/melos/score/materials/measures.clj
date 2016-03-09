(ns melos.score.materials.measures
  (:require [melos.lib.utils :as utils]))

(def measure-1-4
  [[1 4]
   [[[1 8]] [[1 8]]]])

(def measure-2-4
  [[2 4]
   [measure-1-4 measure-1-4]])

(def measure-2-4-triplet
  [[2 4]
   [measure-1-4 measure-1-4 measure-1-4]])

(def measure-3-4
  [[3 4]
   [measure-2-4 measure-1-4]])

(def measure-3-4-quadruplet
  [[3 4]
   [measure-2-4 measure-2-4]])

(def measure-2
  (utils/parse-rtm-tree-node measure-2-4))

(def measure-3
  (let [measure
        [[4 4]
         [[[3 4]
           [measure-2-4
            measure-2-4]]
          measure-2-4]]]
    (utils/parse-rtm-tree-node measure)))

(def measure-4
  (let [measure
        [[4 4]
         [measure-2-4
          measure-2-4]]]
    (utils/parse-rtm-tree-node measure)))

(def measure-5
  (let [measure
        [[7 4]
         [measure-3-4
          measure-3-4-quadruplet
          measure-2-4-triplet]]]
    (utils/parse-rtm-tree-node measure)))

;; (let [divs {[2 4] [1 1 1]}]
;;   divs)

;; (defn newkj

(let [input [[4 4] [1 0] [1] [0]]
      ;; TODO: are these divisions enough?
      ;; TODO: assert that first duration is in divs.
      divs {[4 4] [2 4]
            [2 4] [1 4]
            [1 4] [1 8]}
      curr-timeframe (first input)
      branch-duration (get divs curr-timeframe)
      branch-modifications (second input)
      child-nodes (mapv (fn [mod]
                          (update-in branch-duration [0] (fn [x] (+ mod x))))
                        (take 2 (cycle branch-modifications)))
      ]
  [curr-timeframe
   [child-nodes]])

(defn stretch-tree
  [duration prolations]
  (if (and (seq prolations)
           (contains? {[4 4] [2 4]
                       [2 4] [1 4]
                       [1 4] [1 8]}
                      duration))
    (let [divs {[4 4] [2 4]
                [3 4] [1 4]
                [2 4] [1 4]
                [1 4] [1 8]}
          curr-timeframe duration
          branch-duration (get divs curr-timeframe)
          branch-modifications (first prolations)
          child-nodes (mapv (fn [mod]
                              (update-in branch-duration [0] (fn [x] (+ mod x))))
                            (take 2 (cycle branch-modifications)))
          ]
      (println child-nodes)
      (conj [curr-timeframe]
       (mapv (fn [x] (stretch-tree x (rest prolations))) child-nodes)
       ))
    [duration]))

(defn stretch-tree
  [duration prolations]
  (let [divs {[4 4] [[2 4] [2 4]]
              [3 4] [[2 4] [2 8]]
              [2 4] [[2 8] [2 8]]
              ;; [3 8] [[2 8] [1 8]]
              [2 8] [[1 8] [1 8]]}]
    (if (and (seq prolations)
             (contains? divs duration))
      (let [curr-timeframe duration
            branch-durations (get divs curr-timeframe)
            branch-modifications (first prolations)
            child-nodes (mapv (fn [dur mod]
                                (update-in dur [0]
                                           (fn [x] (+ mod x))))
                              branch-durations
                              (cycle branch-modifications))
            ]
        (println duration)
        (println branch-durations)
        (println child-nodes)
        (println "lkjsdf")
        (conj [curr-timeframe]
              (mapv (fn [x] (stretch-tree x (rest prolations))) child-nodes)
              ))
      [duration])))

(def measure-10
  (utils/parse-rtm-tree-node
   (stretch-tree [4 4] [[1 0] [0] [0] [0] [0] [0]])))

(stretch-tree [4 4] [[1] [0 0] [0] [0] [0] [0]])

;; measure-10

;; (let [measure
;;       [[4 4]
;;        [[[3 4]
;;          [measure-2-4
;;           measure-2-4]]
;;         measure-2-4]]]
;;   measure)

;; [[4 4]
;;  [[[3 4]
;;    [[[2 4] [[[1 4] [[[1 8]] [[1 8]]]] [[1 4] [[[1 8]] [[1 8]]]]]]
;;     [[2 4] [[[1 4] [[[1 8]] [[1 8]]]] [[1 4] [[[1 8]] [[1 8]]]]]]]]
;;   [[2 4] [[[1 4] [[[1 8]] [[1 8]]]] [[1 4] [[[1 8]] [[1 8]]]]]]]]

;; [[4 4]
;;  [[[3 4]
;;    [[[2 4] [[[2 8]] [[2 8]]]] [[2 8] [[[1 8]] [[1 8]]]]]]
;;   [[2 4] [[[2 8] [[[1 8]] [[1 8]]]] [[2 8] [[[1 8]] [[1 8]]]]]]]]

;; (stretch-tree [4 4] [[1 1] [1] [1]])

;; measure-3

;; (stretch-tree [4 4] [[1 0] [0] [0]])

;; TODO: put all materials in the same file.

;; [[2 4]
;;  [[[1 4]
;;    [[[1 8]] [[1 8]]]]
;;   [[1 4]
;;    [[[1 8]] [[1 8]]]]
