(ns melos.scores.graphs.score-graph-1
    (:require [schema.core :as s]
              [plumbing.graph :as graph]
              [plumbing.core :refer [fnk]]
              [melos.tools.selector-sequence :as sel-seq]
              [melos.tools.schemata :as schemata]
              [melos.tools.utils :refer [rotate
                                      ratio->non-reduced-ratio-vector
                                      merge-in]]
              [melos.tools.selector-sequence :refer [collect-events-in-segment]]
              [melos.tools.modify-durations :as mod-dur]
              [melos.tools.default-horizontal-merge :as horizontal-merge]
              [melos.tools.filter-parts :refer [split-out-part]]
              [melos.tools.rtm :as rtm]
              [melos.tools.delay-lines :refer [handle-dissonance]]
              [dire.core :refer [with-handler!]]
              [melos.tools.onsets :refer [get-melodic-event
                                       get-onsets]]))

;; ## Initialize Score

(require '[melos.tools.dissonance-calculator :refer
           [dissonance-value dissonance-value-fn]])

(def segment-graph
  {:melodic-indices
   (fnk [part-seq part->event]
        (sel-seq/get-melodic-segment part-seq
                                     part->event))
   :collect-events
   (fnk [melodic-indices melody-sources duration-scalar]
        (collect-events-in-segment melodic-indices
                                   melody-sources))
   :extend-horizontally
   (fnk [diss-fn-params
         collect-events
         interval->diss-map]
        (swap! dissonance-value
               (fn [x]
                 (dissonance-value-fn interval->diss-map)))
        (let [fn_ (handle-dissonance diss-fn-params)]
          (rest (reductions fn_ [] collect-events))))
   :modified-durations
   (fnk [extend-horizontally mod-dur-patterns]
        (mod-dur/modify-durations extend-horizontally
                                  mod-dur-patterns))
   :merge-horizontally
   (fnk [modified-durations]
        (horizontal-merge/maybe-merge modified-durations))
   :rhythmic-tree
   (fnk [time-signatures merge-horizontally]
        (rtm/make-r-tree merge-horizontally time-signatures))
   :result
   (fnk [part-names modified-durations rhythmic-tree]
        (->> (map
              (fn [part-name] {:part-name part-name
                               :events (split-out-part rhythmic-tree part-name)})
              part-names)
             (rtm/merge-all-tied)))})

(def lazy-segment-graph (graph/lazy-compile segment-graph))

(defn compose-segment
  [segment]
  (lazy-segment-graph segment))
