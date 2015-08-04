(ns melos.scores.graphs.score-graph
    (:require [plumbing.graph :as graph]
              [plumbing.core :refer [fnk]]
              [melos.tools.selector-sequence :as sel-seq]
              [melos.tools.default-horizontal-merge :as horizontal-merge]
              [melos.tools.dissonance-calculator :as diss-calc]
              [melos.tools.delay-lines :as delay-lines]))

(def score->event-seq
  {:events
   (fnk [melodic-indices melody-sources-atom]
        (sel-seq/collect-events-in-segment melodic-indices
                                           melody-sources-atom))
   :melody-sources-atom
   (fnk [melody-sources]
        (atom melody-sources))
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
        ((apply comp mod-dur-patterns) extended-events))
   :merged-horizontally
   (fnk [modified-durations]
        (horizontal-merge/maybe-merge modified-durations))})

(def lazy-segment-graph (graph/lazy-compile score->event-seq))
