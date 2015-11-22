(ns melos.lib.note)

(defn note-default
  []
  {:pitch 10
   :duration 1/4
   :group (gensym "G__")
   :merge-left? false
   :merge-right? false
   :part nil,
   :count 0,
   :is-rest? false
   :max-count 100})

(defn make-note
  "Make a representation of a musical note."
  [params]
  (merge (note-default) params))
