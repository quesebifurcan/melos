(ns melos.tools.make-note
  (:use [schema.core])
  (:require [melos.tools.schemata :as ms]))

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
   :onset 0
   :intensity 1})

(defn make-note
  "Make a representation of a musical note."
  [params]
  (merge (note-default) params))

(defrecord Note
    [pitch :- ms/Pitch
     delta-dur :- Num
     duration :- Num
     dissonance-contributor? :- Bool
     allow-extension? :- Bool
     color :- Any
     merge-left? :- Bool
     merge-right? :- Bool
     group :- Any
     onset :- Num
     is-rest? :- Bool
     part :- ms/PartName
     intensity :- Num
     count :- Int])

(defn make-note-
  [{:keys [pitch
           delta-dur
           duration
           dissonance-contributor?
           allow-extension?
           color
           merge-left?
           merge-right?
           group
           onset
           is-rest?
           part
           intensity
           count]}]
  (validate Note
            (Note. pitch
                   delta-dur
                   duration
                   dissonance-contributor?
                   allow-extension?
                   color
                   merge-left?
                   merge-right?
                   group
                   onset
                   is-rest?
                   part
                   intensity
                   count)))

;; (defn make-note
;;   [params]
;;   (make-note- (merge (note-default)
;;                      params)))
