(ns melos.lib.schemas
  (:require [schema.core :as s]))

(s/defrecord Note
    [count                   :- s/Int
     dissonance-contributor? :- s/Bool
     group                   :- s/Symbol
     is-rest?                :- s/Bool
     max-count               :- s/Int
     merge-left?             :- s/Bool
     merge-right?            :- s/Bool
     notation                :- s/Any
     part                    :- s/Keyword
     pitch                   :- s/Int])

(s/defrecord Chord
    [duration   :- s/Num
     tempo      :- s/Int
     phrase-end :- s/Bool
     events     :- [Note]])

(def Phrase (s/both (s/pred (complement empty?))
                    [Chord]))

(def DurationVector (s/pair s/Int "Numerator" s/Int "Denominator"))

(def Duration s/Num)

(def RhythmTreeNode
  (s/conditional :event
                 {:duration               Duration
                  :sum-of-leaves-duration Duration
                  :chord                  (s/maybe Note)
                  :children               (s/pred nil?)}
                 :children
                 {:duration               Duration
                  :sum-of-leaves-duration Duration
                  :chord                  (s/pred nil?)
                  :children               [(s/maybe (s/recursive #'RhythmTreeNode))]}))

(def DissonanceMapping {s/Num s/Num})

