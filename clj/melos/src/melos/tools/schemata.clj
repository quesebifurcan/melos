(ns melos.tools.schemata)

(use 'plumbing.core)

(require '[plumbing.graph :as graph])
(require '[schema.core :as s])

(require '[scores.event-seqs :as es])

;; TODO: check which types?

(defn pitch-in-range [pitch]
  (<= -100 pitch 200))

(def PartName (s/enum nil :lower :upper :ped))

;; TODO: how deal with the conversion of single pitches into chords?
(def Pitch
  (s/both s/Num
          (s/pred pitch-in-range)))

(def Rest #{:rest})

(def TimeSignatures [s/Num])

(def Event
  {:pitch (s/enum Pitch Rest)
   :onset s/Any
   :part PartName
   :desired-sustain s/Any
   :count s/Num
   :delta-dur s/Any
   :color s/Any})

(def VerticalMoment [s/Any])

;; (def Part->Event
;;   (s/both [s/Keyword]
;;           (s/pred is-valid-melody-reference?)))

;; (defn is-valid-melody-reference [v]
;;   (not (nil? (get-in es/organ v))))
;;   true)

(def Part {PartName s/Any})

(def ScoreSegment
  {:part-seq [PartName]
   :rtm-fn s/Any
   :diss-fn s/Any
   :time-signature-fn s/Any
   :time-signatures s/Any
   :part-names [PartName]
   :part->event s/Any
   :melody-sources s/Any
   :count s/Num})

(def PartialScoreSegment
  (zipmap (map (fn [k] (s/optional-key k))
               (keys ScoreSegment))
          (vals ScoreSegment)))

(def Note
  {:pitch Pitch
   :delta-dur s/Num
   :part PartName
   :count s/Num
   :onset s/Num})

(defn nested-contains?
  [m ks]
  (get-in m ks))

(defn is-valid-melody-reference? [v]
  (not (nil? (get-in (es/organ) v))))

(defn are-valid-melody-references?
  [m]
  (every? is-valid-melody-reference? m))

(def Part->EventMap
  (s/pred are-valid-melody-references?))

(def Part->EventAccessor
  (s/pred is-valid-melody-reference?))

;; (nested-contains? (es/organ) [:lower :a])

;; (s/validate {PartName s/Keyword}
;;             {:lower :b :lkj :bbb})

(s/validate Part->EventAccessor [:lower :a])

(s/validate Part->EventMap
            {:lower :a :upper :a})

;;-----------------------------------------------------------------------------
;; Extending/modifying durations based on dissonance-value
;;-----------------------------------------------------------------------------

(def OrganPitch
  {:delta-dur 1/4
   :inner-dur 1/16
   :extendable? false
   :strict-dur? false})

;; TODO: figure out how to group events.
;; TODO: tempo of segment depends on average dissonance value.
;;-----------------------------------------------------------------------------

;; (every? is-valid-melody-reference {:lower :a :upper :b})

;; (first {:llkj :a})

;; (s/validate {

;; Split functions into two parts -- one defnk form where arguments map
;; is destructured + one schema/defn form which checks the type of the
;; arguments. 

;; TODO: derive rhythmic values from sequences of dissonance-values
;; (or, for a first version, just use the note-count of each vertical
;; moment).

(s/defn ^:always-validate simple-nested
  [events :- [s/Num]
   a      :- s/Num]
  (+ (last events) a))

;; (defnk ^:always-validate simple-nested-fnk
;;   [events
;;    [:args [:nested-fn a]]]
;;   (simple-nested events a))

;; Function arguments -- store in state
;; Multiple operations on one node -- move chain of commands to function.

(def segment-graph
  {:eventsmod (fnk [events [:args [:nested-fn a]]]
                   (simple-nested events a))
   :parts (fnk [events]
               (map (partial + 1) events))
   :jjj (fnk [events eventsmod]
             [events eventsmod])
   :asdf (fnk [eventsmod]
              (do (println "lkjlsdjflksjf") (+ eventsmod 1000)))})
               
(def lazy-segment-graph (graph/lazy-compile segment-graph))

(:jjj (lazy-segment-graph {:events [1 2 3] :args {:nested-fn {:a 123}}}))

;; (lazy-segment-graph {:events [1 2 3] :args {:nested-fn {:a 123}}})

