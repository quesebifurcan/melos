(ns melos.graphs
  (:require [melos
             [chord :as diss-calc]
             [chord-seq :refer [collect-events-in-segment extend-events maybe-merge]]
             [part :as filter-parts]
             [rhythm-tree :as rtm]]
            [plumbing
             [core :refer [fnk]]
             [graph :as graph]]))

(def segment-graph
  {:events
   (fnk [melodic-indices melody-sources-atom]
        (collect-events-in-segment melodic-indices
                                           melody-sources-atom))
   :melody-sources-atom
   (fnk [melody-sources]
        (atom melody-sources))
   :extended-events
   (fnk [events diss-fn-params]
        (extend-events diss-fn-params events))
   :modified-durations
   (fnk [extended-events mod-dur-patterns]
        ((apply comp mod-dur-patterns) extended-events))
   :merged-chord-seq
   (fnk [modified-durations]
        (maybe-merge modified-durations))
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
