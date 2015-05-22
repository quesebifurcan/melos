(ns melos.tools.score-graph
    (:require [schema.core :as s]
              [plumbing.graph :as graph]
              [plumbing.core :as plumbing]
              [melos.tools.selector-sequence :as sel-seq]
              [melos.tools.schemata :as schemata]
              [melos.tools.utils :refer [rotate
                                      ratio->non-reduced-ratio-vector
                                      merge-in]]
              [melos.tools.selector-sequence :refer [collect-events-in-segment]]
              [melos.tools.calc-time-signatures :as time-signatures]
              [melos.tools.modify-durations :as mod-dur]
              [melos.tools.segment-durations :refer [get-durations
                                                  get-segmented-durations
                                                  segment-vertical-moment]]
              [melos.tools.filter-parts :refer [split-out-part]]
              [melos.tools.rtm :as rtm]
              [melos.tools.onsets :refer [get-melodic-event
                                       get-onsets]]))

;; ## Initialize Score

(require '[melos.tools.modify-durations :refer [modify-durations]])

;; TODO: improve naming.
(def segment-graph
  {:melodic-indices
   (plumbing/fnk [part-seq part->event count]
        (sel-seq/get-melodic-segment part-seq part->event count))
   :collected-events
   (plumbing/fnk [melodic-indices melody-sources]
        (collect-events-in-segment melodic-indices melody-sources))
   :dissonance-filtered-events
   (plumbing/fnk [diss-fn collected-events]
        (->> (rest (reductions diss-fn [] collected-events))
             ;; ((fn [x] (map mod-dur/maybe-merge
             ;;               (partition 2 1 [] x))))
             ;; (filter identity)
             ))
   :modified-durations
   (plumbing/fnk [dissonance-filtered-events]
                 (modify-durations dissonance-filtered-events))
   :parts-tree
   (plumbing/fnk [part-names modified-durations]
                 (map (fn [x] {:part-name x
                               :events (split-out-part
                                        modified-durations x)
                               })
                      part-names))})

(def lazy-segment-graph (graph/lazy-compile segment-graph))

(s/defn ^:always-validate compose-segment
  [segment]
  (lazy-segment-graph segment))

