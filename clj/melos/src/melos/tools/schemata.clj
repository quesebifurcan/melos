(ns melos.tools.schemata
  (:require [schema.core :as s]))

(def PartName (s/enum nil :lower :upper :ped))

(def Pitch
  (s/both (s/pred (complement nil?)) s/Num))

(def Note
  {:pitch Pitch
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
   :count s/Int})

(def VerticalMoment
  (s/both (s/pred (complement nil?)) [Note]))

(def PartCountMap
  {PartName s/Int})

(def DissonanceFnParams
  {:max-count s/Int
   :part-counts {PartName s/Int}
   :max-lingering s/Int
   :diss-value s/Num})

(def PartialDissonanceFnParams
  (zipmap (map (fn [k] (s/optional-key k))
               (keys DissonanceFnParams))
          (vals DissonanceFnParams)))

(def DissonanceMapping {s/Num s/Num})

(def ScoreSegment
  ;; TODO: Annotate.
  {:part-seq [PartName]
   :diss-fn-params PartialDissonanceFnParams
   ;; :diss-fn s/Any
   :time-signatures s/Any
   :interval->diss-map DissonanceMapping
   :duration-scalar s/Int
   :part-names [PartName]
   :part->event s/Any
   :melody-sources s/Any
   :count s/Num})

(def PartialScoreSegment
  (zipmap (map (fn [k] (s/optional-key k))
               (keys ScoreSegment))
          (vals ScoreSegment)))
