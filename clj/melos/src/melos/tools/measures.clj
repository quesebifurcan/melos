(ns melos.tools.measures)

(def measure-1
  {:w-duration [5 4]
   ;; :top-level true
   :duration [4 4]
   :children [
              {:w-duration [4 4]
               :duration [3 4]
               :children [
                          {:w-duration [2 4]
                           :duration [2 4]
                           :children [
                                      {:w-duration [1 4]
                                       :duration [1 4]
                                       :event nil
                                       :children nil}
                                      {:w-duration [1 4]
                                       :duration [1 4]
                                       :event nil
                                       :children nil}
                                      ]}
                          {:w-duration [2 4]
                           :duration [2 4]
                           :children [
                                      {:w-duration [1 4]
                                       :duration [1 4]
                                       :event nil
                                       :children nil}
                                      {:w-duration [1 4]
                                       :duration [1 4]
                                       :event nil
                                       :children nil}
                                      ]}
                          ]}
              {:w-duration [2 4]
               :duration [2 4]
               :children [
                          {:w-duration [1 4]
                           :duration [1 4]
                           :event nil
                           :children nil}
                          {:w-duration [1 4]
                           :duration [1 4]
                           :event nil
                           :children nil}
                          ]}
              ]})

(def measure-2
  {:w-duration [5 4]
   :duration [4 4]
   :children [
              {:w-duration [2 4]
               :duration [2 4]
               :children [
                          {:w-duration [1 4]
                           :duration [1 4]
                           :event nil
                           :children nil}
                          {:w-duration [1 4]
                           :duration [1 4]
                           :event nil
                           :children nil}
                          ]}
              {:w-duration [2 4]
               :duration [2 4]
               :children [
                          {:w-duration [1 4]
                           :duration [1 4]
                           :event nil
                           :children nil}
                          {:w-duration [1 4]
                           :duration [1 4]
                           :event nil
                           :children nil}
                          ]}
              ]})

(defn make-node
  ([dur]
  {:duration dur
   :w-duration dur
   :children nil})
  ([dur children]
  {:duration dur
   :w-duration dur
   :children children})
  ([dur w-duration children]
  {:duration dur
   :w-duration w-duration
   :children children}))

(defn regular-subdivs
  []
  (repeat 2 (make-node [1 8]
                       (repeat 2 (make-node [1 16])))))

;; (def measure-3
;;   (clojure.walk/postwalk
;;    (fn [node]
;;      (if (and (map? node)
;;               (nil? (:children node)))
;;        (assoc node :event nil)
;;        node))
;;    (make-node [4 4]
;;               [5 4]
;;               [(make-node [3 4] [3 4]
;;                           (repeat 3 (make-node [1 4])))
;;                (make-node [2 4] [2 4]
;;                           (repeat 2 (make-node [1 4])))])))

(require '[melos.tools.utils :refer [ratio->non-reduced-ratio-vector]])

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

(def measure-3
  (let [measure
        [[3 4]
         ;; [[[2 4]
         ;;   (repeat 2 [[1 4]])]
         ;;  [[3 4]
           [[[2 4]
             (repeat 2 [[1 4]])]
            [[1 4]]]]]
    (parse-rtm-tree-node measure)))
