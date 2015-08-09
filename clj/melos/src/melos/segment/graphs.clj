(ns melos.segment.graphs
  (:require [melos.chord-seq
             [default-horizontal-merge :as horizontal-merge]
             [delay-lines :as delay-lines]
             [selector-sequence :as sel-seq]]
            [melos.chord.dissonance-calculator :as diss-calc]
            [melos.rhythm-tree.rtm :as rtm]
            [melos.tools.filter-parts :as filter-parts]
            [plumbing
             [core :refer [fnk]]
             [graph :as graph]]))

(def segment-graph
  {:events
   (fnk [melodic-indices melody-sources-atom]
        (sel-seq/collect-events-in-segment melodic-indices
                                           melody-sources-atom))
   :melody-sources-atom
   (fnk [melody-sources]
        (atom melody-sources))
   :extended-events
   (fnk [events diss-fn-params]
        (delay-lines/extend-events diss-fn-params events))
   :modified-durations
   (fnk [extended-events mod-dur-patterns]
        ((apply comp mod-dur-patterns) extended-events))
   :merged-chord-seq
   (fnk [modified-durations]
        (horizontal-merge/maybe-merge modified-durations))
   :extended-last
   (fnk [last-event-extension merged-chord-seq]
        (rtm/extend-last last-event-extension merged-chord-seq))
   :rtm-tree
   (fnk [measures extended-last]
        (rtm/make-r-tree measures extended-last))
   :result
   (fnk [rtm-tree tempo part-names]
        {:tempo tempo
         :parts (->> (map
                      (fn [part-name]
                        {:part-name part-name
                         :events (filter-parts/split-out-part rtm-tree part-name)})
                      part-names)
                     (rtm/merge-all-tied))})})

(def segment-graph-compiled (graph/lazy-compile segment-graph))

(defn compose-segment
  [m]
  ((comp :result segment-graph-compiled) m))
