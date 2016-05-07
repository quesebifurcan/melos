(ns melos.lib.schemas
  (:require [schema.core :as s]))

(s/defrecord Note
    [count                   :- s/Int
     dissonance-contributor? :- s/Bool
     duration                :- s/Num
     group                   :- s/Symbol
     is-rest?                :- s/Bool
     max-count               :- s/Int
     merge-left?             :- s/Bool
     merge-right?            :- s/Bool
     notation                :- s/Any
     part                    :- s/Keyword
     pitch                   :- s/Int])

(defn note-default
  []
  {:pitch 0
   :duration 1/4
   :group (gensym "G__")
   :merge-left? false
   :merge-right? false
   :dissonance-contributor? true
   :part :none,
   :notation nil
   :count 0,
   :is-rest? false
   :max-count 100})

(s/defn make-note
  :- Note
  [params :- s/Any]
  (map->Note (merge (note-default) params)))

(s/defrecord Chord
    [duration :- s/Num
     events :- [Note]])

(s/defn make-chord
  []
  (map->Chord {:duration 1/4 :events [(map->Note {})]}))

(def PartName (s/enum nil :lower :upper :ped))

(def Pitch
  (s/both (s/pred (complement nil?)) s/Num))

;; (def Note
;;   (sorted-map :pitch Pitch
;;               :duration s/Num
;;               :merge-left? s/Bool
;;               :merge-right? s/Bool
;;               :group s/Any
;;               :is-rest? s/Bool
;;               :dissonance-contributor? s/Bool
;;               :part s/Keyword
;;               :notation s/Any
;;               :phrase-end s/Bool
;;               :max-count s/Int
;;               :count s/Int))

;; (def Chord
;;   {:duration s/Num
;;    :events [Note]})

;; (def Chord
;;   (s/both (s/pred (complement empty?))
;;           [Note]))

(def Phrase
  (s/both (s/pred (complement empty?))
          [s/Any]))

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
