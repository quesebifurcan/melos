(ns melos.note
  (:require [melos.schemas :as ms]
            [schema.core :as s])
  (:import melos.schemas.Note))

(defn note-default
  []
  {:count 0,
   :dissonance-contributor? true
   :group (gensym "G__")
   :is-rest? false
   :max-count 100
   :merge-left? false
   :merge-right? false
   :notation nil
   :part :none,
   :pitch 0
   :type :Note})

(s/defn make-note
  :- Note
  [params :- s/Any]
  (ms/map->Note (merge (note-default) params)))


