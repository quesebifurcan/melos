(ns melos.schemas
  (:require [schema.core :as s]))

(def PartName (s/enum nil :lower :upper :ped))

(def Pitch
  (s/both (s/pred (complement nil?)) s/Num))

(def Note
  (sorted-map :pitch Pitch
              :duration s/Num
              :merge-left? s/Bool
              :merge-right? s/Bool
              :group s/Any
              :is-rest? s/Bool
              :part PartName
              :max-count s/Int
              :count s/Int))

(def Chord
  (s/both (s/pred (complement nil?)
                  [Note])))

(def PartCountMap
  {PartName s/Int})

(def DissonanceFnParams
  {:max-count s/Int
   :max-lingering s/Int
   :diss-params s/Any})

(def PartialDissonanceFnParams
  (zipmap (map (fn [k] (s/optional-key k))
               (keys DissonanceFnParams))
          (vals DissonanceFnParams)))

(def DissonanceMapping {s/Num s/Num})

(def ScoreSegment
  {:melodic-indices [s/Keyword]
   :diss-fn-params PartialDissonanceFnParams
   :tempo s/Int
   :measures s/Any
   :last-event-extension s/Num
   :mod-dur-patterns s/Any
   :part-names [PartName]
   :melody-sources s/Any})

(def PartialScoreSegment
  (zipmap (map (fn [k] (s/optional-key k))
               (keys ScoreSegment))
          (vals ScoreSegment)))
