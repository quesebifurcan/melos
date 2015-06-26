(ns melos.scores.materials.dissonance-fn-params)

(def diss-fn-params-a
  {:max-count 8
   :part-counts {:upper 2
                 :lower 3
                 :ped 1}
   :max-lingering 5
   :diss-value [0 1 2 3]})

(def presets
  {:a diss-fn-params-a})

(defn retrieve
  [k]
  (presets k))