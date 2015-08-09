(ns melos.params
  (:require [clojure.walk :as walk]
            [melos
             [schemas :as ms]
             [utils :refer [merge-in rotate]]]
            [schema.core :as s]))

;;-----------------------------------------------------------------------------
;; Unfold parameter cycles.

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

;;-----------------------------------------------------------------------------

(defn evaluate-nested-fns
  [state]
  (clojure.walk/postwalk
   (fn [form]
     (if (and (map? form)
              (contains? form :fn)
              (contains? form :params))
       ((:fn form) (:params form))
       form))
   state))
