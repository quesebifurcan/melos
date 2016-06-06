(ns melos.lib.note
  (:require [schema.core :as s]
            [melos.lib.schemas :as ms])
  (:import [melos.lib.schemas Note]))

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


