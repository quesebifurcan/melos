(ns melos.note.make-note)

(defn note-default
  []
  {:pitch 10,
   :delta-dur 1/4
   :duration 1/4
   :group (gensym "G__")
   :merge-left? false
   :merge-right? false
   :dissonance-contributor? true
   :allow-extension? true
   :part nil,
   :color nil,
   :count 0,
   :is-rest? false
   :max-count 100
   :max-part-count 1
   :onset 1
   :intensity 1})

(defn make-note
  "Make a representation of a musical note."
  [params]
  (merge (note-default) params))
