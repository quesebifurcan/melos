(ns scores.event-seqs
  (:require [melos.tools.l-systems :refer [lindenmayer]]
            [melos.tools.make-note :refer [make-note]]))

;; ### The actual musical materials.

(defn make-melody
  [part pitches durations]
  (map (fn [pitch duration]
         (let [group (gensym "G__")]
         [
          (make-note :pitch pitch
                     :delta-dur duration
                     :duration duration
                     :allow-extension? true
                     :group group
                     :merge-right? true
                     :part part
                     :count 0)
          ;; (make-note :pitch (rand-nth [14 0 12 2])
          ;;            :delta-dur duration
          ;;            :group group
          ;;            :part part
          ;;            :count 0)
          ]))
       pitches durations))

(defn make-melody-2
  [part pitches durations]
  (map (fn [pitch duration]
         (let [group (gensym "G__")]
         [
          (make-note :pitch pitch
                     :delta-dur duration
                     :duration duration
                     :group group
                     :dissonance-contributor? true
                     :merge-left? true
                     :part part
                     :count 0)
          ;; (make-note :pitch 9
          ;;            :delta-dur duration
          ;;            :group group
          ;;            :part :lower
          ;;            :count 0)
          ]))
       pitches durations))

(defn pendulum-1 [part]
  (make-melody part
               (cycle [-3 "rest" -2 5 2 "rest" "rest" "rest"
                       3 10 3 2 5 -2])
               ;; (cycle [-5 0 2 0 5 7 2 7 9 0 5 7 5 -2 0 -5 0 2 7 9 7 0 2 -3 2 4])
               (cycle [1/4])))

(defn pendulum-2 [part]
  (make-melody part
               (cycle [-5 0 2 7 2 0])
               ;; (cycle [-5 0 2 0 -5 0 2 7 2 0 -5 -10 -5 0 2 7 2 0 -5 -10 -12 -13])
               (cycle [1/4 1/4 1/4 1/4 1/4 1/4 1/4 1/4])))

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

(defn organ
  []
  ;; TODO: return lazy seqs.
  {:upper {:a (pendulum-1 :upper)}
   :lower {:a (pendulum-2 :lower)}
   :ped {:a (lindenmayer-1 :ped 10)
         :b (lindenmayer-1 :ped 7)}})

