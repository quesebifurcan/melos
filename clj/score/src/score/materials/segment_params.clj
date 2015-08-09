(ns score.materials.segment-params
  (:require [schema.core :as s]
            [melos.schemas :as ms]))

(s/defn initial-state
  :- ms/ScoreSegment
  []
  {:melody-sources {:upper []
                    :lower []
                    :ped []}
   :diss-fn-params {:max-count 10
                    :max-lingering 20
                    :diss-value [0 4 7]}
   :measures []
   :mod-dur-patterns []
   :tempo 240
   :last-event-extension 7/4
   :part-names [:upper :lower :ped]
   :melodic-indices []})

(initial-state)
