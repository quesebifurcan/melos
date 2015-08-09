(ns melos.schemas.schemas
  (:require [schema.core :as s]))

(def PartName (s/enum nil :lower :upper :ped))

(def Pitch
  (s/both (s/pred (complement nil?)) s/Num))

(def Note
  (sorted-map :pitch Pitch
              :delta-dur s/Num
              :duration s/Num
              :dissonance-contributor? s/Bool
              :allow-extension? s/Bool
              :color s/Any
              :merge-left? s/Bool
              :merge-right? s/Bool
              :group s/Any
              :onset s/Num
              :is-rest? s/Bool
              :part PartName
              :intensity s/Num
              :max-count s/Int
              :max-part-count s/Int
              :count s/Int))

(def Chord
  (s/both (s/pred (complement nil?)
                  [Note])))

(def PartCountMap
  {PartName s/Int})

(def DissonanceFnParams
  {:max-count s/Int
   :max-lingering s/Int
   :diss-value [s/Num]})

(def PartialDissonanceFnParams
  (zipmap (map (fn [k] (s/optional-key k))
               (keys DissonanceFnParams))
          (vals DissonanceFnParams)))

(def DissonanceMapping {s/Num s/Num})

(def ScoreSegment
  {:melodic-indices [s/Keyword]
   :diss-fn-params PartialDissonanceFnParams
   :time-signatures s/Any
   :interval->diss-map DissonanceMapping
   :tempo s/Int
   :mod-dur-patterns s/Any
   :part-names [PartName]
   :melody-sources s/Any})

(def PartialScoreSegment
  (zipmap (map (fn [k] (s/optional-key k))
               (keys ScoreSegment))
          (vals ScoreSegment)))
