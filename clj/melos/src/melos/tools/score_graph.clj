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
              [melos.tools.modify-durations :as mod-dur]
              [melos.tools.filter-parts :refer [split-out-part]]
              [melos.tools.rtm :as rtm]
              [melos.tools.delay-lines :refer [handle-dissonance]]
              [dire.core :refer [with-handler!]]
              [melos.tools.onsets :refer [get-melodic-event
                                       get-onsets]]))

;; ## Initialize Score

(require '[melos.tools.modify-durations :refer [modify-durations]])
(require '[melos.tools.dissonance-calculator :refer
           [dissonance-value dissonance-value-fn]])

(defn scale-durations
  [events scale-factor]
  (clojure.walk/postwalk
   (fn [event]
     (if (and (map? event)
              (contains? event :delta-dur))
       (update-in event [:duration] (fn [x] (* scale-factor x)))
       event))
   events))

;; TODO: improve naming.
(def segment-graph
  {:melodic-indices
   (plumbing/fnk [part-seq part->event count]
        (sel-seq/get-melodic-segment part-seq part->event count))
   :collected-events
   (plumbing/fnk [melodic-indices melody-sources duration-scalar]
                 (->> (collect-events-in-segment melodic-indices melody-sources)
                     (map #(scale-durations % duration-scalar))))
   :dissonance-filtered-events
   (plumbing/fnk [diss-fn-params
                  collected-events
                  interval->diss-map]
                 (swap! dissonance-value (fn [x]
                                            (dissonance-value-fn
                                             interval->diss-map)))
                 (let [fn_ (handle-dissonance diss-fn-params)]
                   (->> (rest (reductions fn_ [] collected-events))
                        )))
   :modified-durations
   (plumbing/fnk [dissonance-filtered-events]
                 (modify-durations dissonance-filtered-events))
   :rhythmic-tree
   (plumbing/fnk [time-signatures modified-durations]
                 (rtm/calculate-result modified-durations
                                       time-signatures))
   :parts-tree
   (plumbing/fnk [part-names modified-durations rhythmic-tree]
                 (map (fn [x] {:part-name x
                               :events (split-out-part rhythmic-tree x)
                               })
                      part-names))})

(def lazy-segment-graph (graph/lazy-compile segment-graph))

(defn compose-segment
  [segment]
  (lazy-segment-graph segment))
