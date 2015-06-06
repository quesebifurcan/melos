(ns scores.materials.event-seqs
  (:require [melos.tools.l-systems :refer [lindenmayer]]
            [melos.tools.make-note :refer [make-note]]))

;; ### The actual musical materials.
(defn make-melody
  [part pitches durations is-rest?]
  (map (fn [pitch duration is-rest?]
         (let [group (gensym "G__")]
           [[
             (make-note {:pitch pitch
                         :delta-dur duration
                         :duration duration
                         :allow-extension? true
                         :group group
                         :merge-right? true
                         :is-rest? is-rest?
                         :part part
                         :count 0})
             ]
            ;; (make-note {:pitch 24
            ;;            :delta-dur duration
            ;;            :duration duration
            ;;            :allow-extension? true
            ;;            :group group
            ;;            :merge-right? true
            ;;            :part part
            ;;            :count 0})]
            ]))
       pitches durations is-rest?))

(defn make-melody-2
  [part pitches durations]
  (map (fn [pitch duration]
         (let [group (gensym "G__")]
         [[
          (make-note {:pitch pitch
                     :delta-dur duration
                     :duration duration
                     :group group
                     :dissonance-contributor? true
                     :merge-left? false
                     :merge-right? false
                     :part part
                     :is-rest? false
                     :count 0})
          ;; (make-note {:pitch 9
          ;;            :delta-dur duration
          ;;            :group group
          ;;            :part :lower
          ;;            :count 0})
          ]]))
       pitches durations))

(defn pendulum-1 [part partitioning]
  (->> (make-melody part
                    (cycle [-3 -2 5 2
                            3 10 3 2 5 -2])
                    ;; (cycle (range -3 10))
                    (cycle [1/4])
                    (cycle [false]))
       (flatten)
       (partition 1)
       (partition partitioning)
       ))


(defn pendulum-2 [part]
  (make-melody part
               (cycle [-5 0 2 7 2 0])
               ;; (cycle [-5 0 2 0 -5 0 2 7 2 0 -5 -10 -5 0 2 7 2 0 -5 -10 -12 -13])
               (cycle [1/4 1/4 1/4 1/4 1/4 1/4 1/4 1/4])
               (cycle [false])))

(defn lindenmayer-1 [part offset]
  (let [pitches (->> (lindenmayer {1 [1 -2] -2 [7 -3] -3 [2 1]}
                                  10
                                  [-2])
                     (reductions + -5)
                     (map #(rem % 12))
                     (map #(- % offset)))]
    (make-melody-2 part
                 pitches
                 (cycle [4/4]))))

;; (require '[scores.test-event-seq :as tes])
;;                                          alternating-pitch-rest
;;                                          morph
;;                                          unfold-events]])

(defn organ
  []
  ;; TODO: return lazy seqs.
  {:upper {:a (pendulum-1 :upper 3)}
  ;; {:upper {:a (tes/unfold-events (tes/morph))}
   :lower {:a (pendulum-2 :lower)}
   :ped {:a (lindenmayer-1 :ped 22)
         :b (lindenmayer-1 :ped 7)}})


(take 10 (pendulum-1 :upper 3))
