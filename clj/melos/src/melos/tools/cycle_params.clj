(ns melos.tools.cycle-params
  (:require [clojure.walk :as walk]
            [schema.core :as s]
            [melos.tools.schemata :as ms]
            [melos.tools.utils :refer [rotate
                                       merge-in]]))

(defn is-part-of-seq
  [s i]
  (let [cycle-dur (apply + s)
        i (rem i cycle-dur)]
    (some #{i} (reductions + 0 s))))

(defn is-active-parameter?
  [form i]
  (and (map? form)
       (contains? form :path)
       (is-part-of-seq (:cycle form) i)))

(defn current-value [{:keys [path values]}]
  {path (first values)})

(defn transform-and-collect-event-if-active-at-index
  [state i]
  (let [coll (atom {})
        new-state (walk/postwalk
                   (fn [form] (if (is-active-parameter? form i)
                                (do (swap! coll #(merge % (current-value form)))
                                    (update-in form [:values] rotate))
                                form))
                   state)]
    [new-state coll]))

(defn maybe-coll-change
  [state i]
  (let [[new-state coll]
        (transform-and-collect-event-if-active-at-index state i)]
    (lazy-seq (cons @coll
                    (maybe-coll-change new-state (inc i))))))

(defn create-nested-map
  [vector-path-map]
  (if (empty? vector-path-map)
    {}
    (reduce merge-in
            (map (fn [[k v]]
                   (assoc-in {} k v))
                 vector-path-map))))

(s/defn unfold-parameter-cycles
  :- s/Any
  [params :- [{s/Keyword s/Any}]
   cnt :- s/Int]
  (->> (maybe-coll-change params 0)
       (map create-nested-map)
       (take cnt)))

;; TODO: allow-extension part of segment composition?

(let [a [
         ;; {:path [:part-seq]
         ;;  :cycle [1 1 2]
         ;;  :values [[:lower :upper] [:upper :upper :lower :ped]]}
         ;; {:path [:part->event :upper]
         ;;  :cycle [2 3]
         ;;  :values [:a :b :c]}
         {:path [:part->event :upper]
          :cycle [4 4]
          :values [:chromatic-line :chords]}
         {:path [:part->event :ped]
          :cycle [2 3 4 5 6]
          :values [:ascending :descending :chords]}
         {:path [:interval->diss-map]
          :cycle [5 2]
          :values [:default :experimental]}
         {:path [:count]
          :cycle [1]
          :values [20 50 100]}
         ]]
  (->> (unfold-parameter-cycles a 20)
       (reductions merge-in)
       (map :count)
       (apply +)
       (* 0.5)
       ((fn [x] (/ x 60)))
       ))

