(ns melos.scores.graphs.score-graph
    (:require [plumbing.graph :as graph]
              [plumbing.core :refer [fnk]]
              [melos.tools.selector-sequence :as sel-seq]
              [melos.tools.modify-durations :as mod-dur]
              [melos.tools.default-horizontal-merge :as horizontal-merge]
              [melos.tools.dissonance-calculator :as diss-calc]
              [melos.tools.filter-parts :as filter-parts]
              [melos.tools.rtm :as rtm]
              [melos.tools.delay-lines :as delay-lines]))

(def segment-graph
  {:events
   (fnk [melodic-indices melody-sources]
        (sel-seq/collect-events-in-segment melodic-indices
                                           melody-sources))
   :extended-events
   (fnk [events diss-fn-params interval->diss-map]
        ;; Set dissonance-map (globally).
        ;; TODO: more elegance.
        (swap! diss-calc/dissonance-value
               (fn [_]
                 (diss-calc/dissonance-value-partial interval->diss-map)))
        (let [fn_ (delay-lines/handle-dissonance diss-fn-params)]
          (rest (reductions fn_ [] events))))
   :modified-durations
   (fnk [extended-events mod-dur-patterns]
        (mod-dur/modify-durations extended-events
                                  mod-dur-patterns))
   :merged-horizontally
   (fnk [modified-durations]
        (horizontal-merge/maybe-merge modified-durations))
   :rhythmic-tree
   (fnk [time-signatures merged-horizontally]
        (rtm/make-r-tree merged-horizontally
                         time-signatures))
   :result
   (fnk [tempo part-names modified-durations rhythmic-tree]
        {:tempo tempo
         :parts (->> (map
                      (fn [part-name]
                        {:part-name part-name
                         :events (filter-parts/split-out-part rhythmic-tree part-name)})
                      part-names)
                     (rtm/merge-all-tied))})})

(def lazy-segment-graph (graph/lazy-compile segment-graph))

(defn compose-segment
  [segment]
  (lazy-segment-graph segment))
