(ns melos.lib.note
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

(defn make-note
  [params]
  (map->Note (merge (note-default) params)))
