(ns melos.scores.graphs.score-graph
    (:require [plumbing.graph :as graph]
              [plumbing.core :refer [fnk]]
              [melos.tools.selector-sequence :as sel-seq]
              [melos.tools.rtm :as rtm]
              [melos.tools.filter-parts :as filter-parts]
              [melos.tools.default-horizontal-merge :as horizontal-merge]
              [melos.tools.dissonance-calculator :as diss-calc]
              [melos.tools.delay-lines :as delay-lines]))

(def score-segment->event-seq
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
   :result
   (fnk [modified-durations]
        (horizontal-merge/maybe-merge modified-durations))})

(def event-seq->rtm-tree
  {:extended-events 
   (fnk [last-event-extension event-seq]
        (rtm/extend-last last-event-extension event-seq))
   :rtm-tree
   (fnk [measures extended-events]
        (rtm/make-r-tree measures extended-events))
   :result
   (fnk [rtm-tree tempo part-names]
        {:tempo tempo
         :parts (->> (map
                      (fn [part-name]
                        {:part-name part-name
                         :events (filter-parts/split-out-part rtm-tree part-name)})
                      part-names)
                     (rtm/merge-all-tied))})})

(def lazy-segment-graph (graph/lazy-compile score-segment->event-seq))
(def lazy-rtm-graph (graph/lazy-compile event-seq->rtm-tree))
