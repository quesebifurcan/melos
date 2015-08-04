(ns melos.scores.segments.unfold-event-seq)

(defn- update-state
  [initial-state updates]
  (reduce (fn [m [k v]]
            (update-in m k (fn [_] v)))
          initial-state
          updates))

(defn- evaluate-nested-fns
  [state]
  (clojure.walk/postwalk
   (fn [form]
     (if (and (map? form)
              (contains? form :fn)
              (contains? form :params))
       ((:fn form) (:params form))
       form))
   state))

(defn unfold-events
  [initial-state updates]
  (->> (update-state initial-state updates)
       (evaluate-nested-fns)))
