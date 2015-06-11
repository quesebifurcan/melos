(ns melos.scores.test-event-seq
  (:require [clojure.walk :as walk]
            [melos.tools.dissonance-calculator :refer
             [dissonance-map-default dissonance-map-2]]
            [melos.scores.materials.measures :as measures]
            [melos.tools.utils :refer [export-to-json]]

            [melos.tools.l-systems :refer [lindenmayer]]
            [melos.tools.make-note :refer [make-note]]
            [melos.tools.modify-durations :as mod-dur]
            [melos.tools.rtm :refer [make-r-tree merge-all-tied]]
            [melos.tools.contour :refer
             [apply-contour-to-melody]]
            [melos.scores.materials.measures :refer [measure-3]]
            [melos.scores.main :refer [compose-score]]
            [melos.tools.utils :refer [rotate]]
            [melos.tools.filter-parts :refer [split-out-part]]
            [melos.tools.utils :refer [export-to-json]]))

(defn compose-single-line
  [events]
  (-> (map first events)
      (make-r-tree [measure-3])
      (merge-all-tied)
      ))

(defn export-single-event-seq [part-name events]
  (let [result (compose-single-line events)]
    (export-to-json "/Users/fred/Desktop/score.json"
                    [[{:part-name :upper
                       :events (split-out-part result :upper)}]])))

                    ;; (
                    ;; [[{:part-name part-name :events result}]])))

(defn make-chord-from-pitch-vector-params
  [{:keys [pitch] :as m}]
  (let [group (gensym "G__")]
  (map (fn [p]
         (make-note (merge {:pitch p :group group}
                           (dissoc m :pitch))))
       pitch)))

(defn parse-params
  [x]
  (if (list? x)
    (repeat (last x) (first x))
    [x]))

(defn unfold-events
  [m part]
  (let [m (assoc m :part [part])
        f (:fn m)
        partition-fn (:partition m)
        m (dissoc m :fn :partition)]
  (->> (map (fn [x] (mapcat parse-params x))
            (vals m))
       (map cycle)
       (apply map vector)
       (map (fn [x] (zipmap (keys m) x)))
       (map f)
       (partition-fn))))

(defn split-if-chord
  [events]
  (clojure.walk/prewalk
   #(if (and (map? %)
             (vector? (:pitch %)))
      (let [pitches (:pitch %)]
        (into [] (map (fn [x] (assoc % :pitch x)) pitches)))
      %)
   events))

(def alternating-pitch-rest
  {:pitch (mapcat (fn [x] [x "rest"])
                  (range 0 10))
   :dissonance-contributor? [false]
   :part [:upper]
   :fn (fn [x] [(make-note x)])
   :partition (fn [x] (partition 1 x))
   :duration [1/4 2/4]})

;; (def chords
;;   {:pitch ['([0 2 4] 1) [3 10 14]]
;;    :dissonance-contributor? [false]
;;    :part [:upper]
;;    :fn (fn [x] (->> (make-note x)
;;                     (split-if-chord)
;;                     ;; (map split-if-chord)))
;;                     ))
;;    :partition (fn [x] (partition 1 x))
;;    :duration [1/4 2/4 1/4 2/4 1/4 3/4 5/4 1/4]})

(defn apply-contour
  [pitches]
  (->> (map (fn [x] (rem x 12)) pitches)
       ;; (sort)
       ;; ((fn [x] (concat x (reverse (butlast (rest x))))))))
       ))

(defn transpose-motif-gradually
  [pitches]
  (let [transpositions (reductions + 0 (repeat 12 7))
        tails (map (fn [increment]
                     (map (fn [x] (+ x increment))
                          (rest pitches)))
                   transpositions)
        result (map (fn [x] (concat [(first pitches)] x))
                    tails)]
    (->> (map apply-contour result)
         (flatten))))

(transpose-motif-gradually [0 2 7])

;; transpose by fifths, one note at a time.
;; repeat sections (after the contour has been applied).

(defn repeat-segments
  [motif split repeat-n]
  ;; TODO: different ways to partition motif.
  (let [segments (partition split motif)]
    (flatten (mapcat (fn [x] (repeat repeat-n x))
                     segments))))

(defn transpose-motif-by-fifths-
  [motif index-seq]
  (let [result (update-in motif
                          [(rem (first index-seq) (count motif))]
                          (fn [x] (rem (+ x 7) 12)))]
    (lazy-seq (concat result (transpose-motif-by-fifths- result (rotate index-seq))))))

(defn transpose-motif-by-fifths
  [motif index-seq]
  (concat motif (transpose-motif-by-fifths- motif index-seq)))

(defn morph-pitches
  []
  (-> (transpose-motif-by-fifths [0 2 7] [1 2])
      (repeat-segments 6 3)
      ;; N.B. gradually rising melody.
      ;; TODO: cache results.
      ;; (apply-contour-to-melody (cycle (concat (range 0.11 10 0.1)
      ;;                                         (range 10 0.11 -0.1))))

      (flatten)
      ))

;; mix formalized and non-strict melody sources.

(require '[melos.tools.utils :refer [rotate]])

(defn cyclic-partition
  [xs splits]
  (cons (take (first splits) xs)
        (lazy-seq (cyclic-partition (drop (first splits) xs)
                                    (rotate splits)))))
(defn morph
  []
  {:pitch (morph-pitches)
   :part [:upper]
   :fn (fn [x] [(make-note x)])
   :partition #(cyclic-partition % [2 4 3])
   :duration [1/4]})

(defn delimit-phrases-with-rests
  [event-groups]
  (map (fn [x]
         (conj x [(make-note {:is-rest? true
                              :part :upper
                              :group 1})]))
       event-groups))

(defn cyclic-repeats
  [xs repeats]
  (if (seq xs)
    (concat (repeat (first repeats) (first xs))
            (lazy-seq (cyclic-repeats (rotate xs)
                                      (rotate repeats))))))

(defn upper-soft
  []
  ;; {:pitch (map (fn [x] [x])
  ;;              [-3 -2 5 2 3 10 3 2 5 -2])
  ;; {:pitch (map (fn [x] [x])
  ;;              (concat
  ;;               (range -3 10)
  ;;               (range 10 -3 -1)))
  {:pitch
   (map-indexed (fn [i x]
                  (if (= (rem i 9) 0)
                    [x] [x]))
                (concat
                 (range -3 10)
                 (range 10 -3 -1)))
   :dissonance-contributor? [true]
   :part [:upper]
   :fn make-chord-from-pitch-vector-params
   :partition #(cyclic-partition % [1 1 1 2 2 1 1 2])
   :duration [1/4 1/4 1/4 1/4]})

(defn complement-upper
  []
  ;; {:pitch [-5 0 2 7 2 0
  ;;          -5 0 2 7 12 14 12 7 2 0]
  {:pitch (map (fn [x] (- x 0))
               ;; [-1 11 6])
               ;; [-2 10 5])
               [-1 11 6])
   :part [:upper]
   :fn (fn [x] [(make-note x)])
   :partition #(cyclic-partition % [1])
   :duration [1/4]})

(defn extended-bass
  []
  ;; {:pitch ['(-17 1) '(-19 2) '(-21 2) '(-22 3)]
  {:pitch (range -17 -22 -1)
   :part [:upper]
   ;; :dissonance-contributor? [false]
   :fn (fn [x] [(make-note x)])
   :partition #(cyclic-partition % [1])
   :duration ['(1/4 7) '(5/4 1)]})

(defn tie-over
  []
  {:pitch (map (fn [x] (if (number? x) [x] x))
                 [-19 [-19 -15] [-19 -5] [-5 -7] [-5 -9]])
   :dissonance-contributor? [true]
   :part [:upper]
   :fn (fn [x] (->> (make-note x)
                    (split-if-chord)))
   :partition #(cyclic-partition % [1])
   :duration ['(2/4 7) 4/4]})

(defn organ-materials
  []
  {:b (unfold-events (upper-soft) :upper)
   :e (unfold-events (complement-upper) :lower)
   :d (unfold-events (extended-bass) :ped)
   })

(defn test-score-segment
  []
  (let [materials (organ-materials)]
  {:part-seq (take 200
                   (cycle [:lower
                           :upper
                           :lower
                           :upper
                           :ped
                           ]))
   :diss-fn-params {:max-count 10
                    :part-counts {:upper 1
                                  :lower 2
                                  :ped 1}
                    :max-lingering 30
                    :diss-value [0 4 6]}
   :interval->diss-map dissonance-map-default
   :part->event {:upper :primary
                 :lower :primary
                 :ped :primary}
   ;; TODO: pass in via score-graph.
   :time-signatures [measures/measure-3]
   :duration-scalar 1
   :mod-dur-patterns [mod-dur/dissonant-melody-movement-mod]
   ;; :mod-dur-patterns []
   :part-names [:upper :lower :ped]
   :melody-sources (atom
                    {:upper {:primary (:b materials)}
                     :ped {:primary (:d materials)}
                     :lower {:primary (:e materials)}})
   :count 0}))


(time
 (export-to-json "/Users/fred/Desktop/score.json"
                 (compose-score (test-score-segment)
                                [{}])))

;; (first (unfold-events (asdf)
;;                       :upper))

;; (take 10 (unfold-events (asdf)))

;; (time
;;  (->> (take 800 (unfold-events (morph)))
;;       (export-single-event-seq :upper)
;;       ))

;; Rests?

;; (:pitch (morph))

;; (take 10 (unfold-events (morph)))

;; ;; (take 1000 (morph-pitches)))

;; ;; (apply-contour-to-melody
;; ;;  [12 1 12 3]
;; ;;  [0 0 0 0])
