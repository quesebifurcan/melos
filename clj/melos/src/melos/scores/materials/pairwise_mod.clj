(ns melos.scores.materials.pairwise-mod
  (:require [melos.chord-seq.modify-durations :as mod-dur]
            [melos.chord.dissonance-calculator :refer [scaled-dissonance-value]]))

(defn dissonant-melody-movement-mod
  [pair]
  (let [melodic-notes (mapcat
                       (fn [x] (filter #(= (:count %) 0) x))
                       pair)
        melodic-notes-1 (filter :dissonance-contributor?
                              melodic-notes)
        ;; melodic-notes (partition-by :part melodic-notes-1)
        ;; melodic-pitches (map
        ;;                (fn [x]
        ;;                  (first (filter number? (map :pitch x))))
        ;;                    melodic-notes)]
        melodic-pitches (map :pitch melodic-notes-1)]
    ;; (println (map :part melodic-notes-1))
    ;; (println (map :part melodic-notes-1))
    ;; (println melodic-pitches)
    (if (and (>= (scaled-dissonance-value melodic-pitches)
                 (scaled-dissonance-value [0 1]))
             (> (count (set (map :part melodic-notes-1))) 1))
      (let [[a b]
            (map (fn [events dur]
                   (map (fn [event]
                          (assoc event
                                 :delta-dur dur
                                 :duration dur))
                        events))
                 pair
                 [3/4 2/4])]
        [a b]))))

(defn sustain-dissonant-melody
  [events]
  (mod-dur/modify-durations events [dissonant-melody-movement-mod]))

;; (defn parts-count-drop
;;   [pair]
;;   (if (and (>= (count (first pair)) 1)
;;            (= (count (second pair)) 1))
;;     (let [[a b]
;;           (map (fn [events dur]
;;                  (map (fn [event]
;;                         (assoc event
;;                                :delta-dur dur
;;                                :duration dur))
;;                       events))
;;                pair
;;                [3/4 2/4])]
;;       [a b])))

;; (defn sustain-parts-count-drops
;;   [events]
;;   (mod-dur/modify-durations events [parts-count-drop]))


