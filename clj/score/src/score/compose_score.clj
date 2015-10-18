(ns score.compose-score
  (:require [clojure.math.combinatorics :as combinatorics]
            [melos
             [note :as note]
             [chord :as chord]
             [chord-seq :as chord-seq]
             [rhythm-tree :as rhythm-tree]
             [part :as part]
             [schemas :as ms]
             [params :as params]
             [utils :as utils]]
            [schema.core :as s]
            [score.combinations :as combinations]
            [score.materials
             [measures :as measures]
             [stepwise-mod :as stepwise-mod]]))

(use 'clojure.data)
(require 'clojure.edn)

;;-----------------------------------------------------------------------------
;; SCORE

(defn compose-segment
  [{:keys [events
           diss-fn-params
           pre
           post
           measures
           tempo
           part-names]}]
  (->> events
       ((apply comp pre))
       (chord-seq/extend-events diss-fn-params)
       (chord-seq/merge-horizontally)
       ((apply comp post))
       (rhythm-tree/make-r-tree measures)
       (part/compose-part tempo part-names)))

(defn compose-event-seq
  [{:keys [events
           diss-fn-params
           pre
           post]}]
  (->> events
       ((apply comp pre))
       (chord-seq/extend-events diss-fn-params)
       (chord-seq/merge-horizontally)
       ((apply comp post))))

(defn compose-parts
  [measures tempo part-names event-seq]
  (let [head (stepwise-mod/maybe-split-vertical-moment
              (first event-seq))
        event-seq-mod (concat head (rest event-seq))]
    (->> event-seq-mod
         (rhythm-tree/extend-last 3/4)
         (rhythm-tree/make-r-tree measures)
         (part/compose-part tempo part-names))))

(defn make-overlaps [pitches]
  (->> pitches
       (partition 2 1)
       (mapcat (fn [[x y]]
                 [[x] [x y]]))))

(defn diatonic-ped
  [part-name transposition]
  {:pitch (->> [0 2 4 5 7 9]
               (utils/transpose transposition)
               (make-overlaps))
   :part [part-name]
   :fn utils/make-chord-from-pitch-vector-params
   :partition (partial utils/cyclic-partition [2])
   :max-part-count [2]
   :duration [1/4]})

(defn upper
  [part-name transposition dur]
  {:pitch [
           ;; [0] [0 7] [0 7 12] [0 7 12] [7 12] [12]
           ;; [0] [0 2] [0 2 5] [0 2 5]
           [0] [0 7] [0 7 14] [0 7 9 14]
           ]
   :part [part-name]
   :fn utils/make-chord-from-pitch-vector-params
   :partition (partial utils/cyclic-partition [1])
   :max-part-count [4]
   :merge-left? [true]
   :merge-right? [true]
   :duration [dur]})

;; (first
;;  (utils/unfold-events (upper :upper 0)))

(defn diatonic-ped
  [pitches part-name transposition]
  {:pitch (->> pitches
               (utils/transpose transposition)
               (map (fn [x] [x])))
               ;; (make-overlaps))
   :part [part-name]
   :fn utils/make-chord-from-pitch-vector-params
   :partition (partial utils/cyclic-partition [2])
   :max-part-count [1]
   ;; :max-lingering [200]
   :merge-left? [true]
   :merge-right? [true]
   :duration [1/4]})

(defn diatonic-ped-2
  [part-name transposition]
  ;; {:pitch (->> (range 8 0 -1)
  {:pitch (->> (range -4 8)
               (utils/transpose transposition)
               (map (fn [x] [x])))
   :part [part-name]
   :fn utils/make-chord-from-pitch-vector-params
   :partition (partial utils/cyclic-partition [1])
   :max-part-count [1]
   :merge-left? [true]
   :merge-right? [true]
   :duration [1/4]})

;; (def event-seqs
;;   {:upper {:pitch [[0] [0 7] [0 7 12] [0 7 12] [7 12] [12] [-1] [-1 0] [-1 0 2]]
;;            :part [:upper]
;;            :fn utils/make-chord-from-pitch-vector-params
;;            :partition (partial utils/cyclic-partition [1])
;;            :max-part-count [3]
;;            :merge-left? [true]
;;            :merge-right? [true]
;;            :duration [1/4]}
;;    :lower {:pitch (->> pitches
;;                        (utils/transpose transposition)
;;                        (map (fn [x] [x])))
;;            ;; (make-overlaps))
;;            :part [part-name]
;;            :fn utils/make-chord-from-pitch-vector-params
;;            :partition (partial utils/cyclic-partition [2])
;;            :max-part-count [1]
;;            ;; :max-lingering [200]
;;            :merge-left? [true]
;;            :merge-right? [true]
;;            :duration [1/4]}
;;    :ped {:pitch (->> [0 2 11 17 16 1 1 1 1 1 1]
;;                      (utils/transpose 2)
;;                      (map (fn [x] [x])))
;;          :part [part-name]
;;          :fn utils/make-chord-from-pitch-vector-params
;;          :partition (partial utils/cyclic-partition [1])
;;          :max-part-count [1]
;;          :merge-left? [true]
;;          :merge-right? [true]
;;          :duration [1/4]}

;; (let [a [
;;          [[0] [0 1] [0 2]]
;;          [[2] [2 3] [2 4]]
;;          ]
;;       grp (fn [x] [x])
;;       b (map grp (range 20))
;;       c [0 7 12 0 7 12 14 2 7 2 7 9 14 2 7 9 12 0 7]
;;       d (map (comp grp grp) c)
;;       e [[7] [7 5] [7 4] [4 5] [3 5] [2 5] [2 4] [4 1]]
;;       f [[0] [0 12] [0 9 12] [0 7 12] [0 7 9 12]]
;;       ]
;;   f)

(defn make-phrase
  [chords]
  (map (fn [chord]
         (let [group (gensym "G__")]
           (map (fn [pitch]
                  (note/make-note {:pitch pitch :group group :part :upper :max-part-count 3}))
                chord)))
       chords))

(defn split-at-pcs
  [scale]
  (partition-by (fn [pitch] (contains? #{0 2 4 7} pitch)) scale))

;; (->>
;;  (partition 3
;;             3
;;             []
;;             (split-at-pcs (range 0 12)))
;;  (map #(apply concat %)))

(defn build-chord
  [pitches]
  (rest (reductions conj [] pitches)))

(def test-pitches
  (->> [[0 2] [2 4] [4 7] [4 6]]
       (map (fn [x] (apply range x)))
       (map build-chord)
       (map make-phrase)))

(def materials
  {:upper
   [(utils/unfold-events (upper :upper -1 1/4))]
   :lower (map (fn [offset] (drop offset
                                  (utils/unfold-events (diatonic-ped (range 10) :lower -7))))
                                  (range 8))
   :ped (map (fn [offset] (drop offset
                                (utils/unfold-events (diatonic-ped-2 :ped -20))))
             (range 20))
   :melodic-indices [(take 20 (cycle [:upper :lower :ped]))]
   })

(def materials-2
  {:upper
   [(utils/unfold-events (upper :upper 8 1/4))]
   :lower (map (fn [offset] (drop offset
                                  (utils/unfold-events (diatonic-ped (range 10) :lower -7))))
               (range 8))
   :ped (map (fn [offset] (drop offset
                                (utils/unfold-events (diatonic-ped-2 :ped -13))))
             (range 20))
   :melodic-indices [(take 20 (cycle [:upper :lower :ped]))]
   })

(defn make-chord-seq
  [{:keys [upper lower ped melodic-indices]}]
  (let [melody-sources-atom (atom {:lower lower
                                   :upper upper
                                   :ped ped})]
    (chord-seq/collect-events-in-segment melodic-indices
                                         melody-sources-atom)))

(defn chord-seqs
  [source]
  (->> source
       (combinations/unfold-parameters)
       (map make-chord-seq)))

(def diss-params
  {:check (fn [events]
            (<= (chord/scaled-dissonance-value (map :pitch events))
                (chord/scaled-dissonance-value [0 2 4 5])))})
                ;; 1000))})

(defn initial-state
  [events]
  {:events events
   :diss-fn-params {:max-count 100
                    :max-lingering 300
                    ;; TODO: pass on diss-value
                    :diss-params diss-params}
   :tempo 200
   :measures [measures/measure-4]
   :pre []
   ;; :post [(partial rhythm-tree/extend-last 7/4)]
   :post []
   :part-names [:upper :lower :ped]})

(defn parts-in-chord
  [chord]
  (set (map :part chord)))

(defn part-count-sufficient?
  [minimum chord]
  (let [part-count ((comp count parts-in-chord) chord)]
    (= part-count minimum)))

(def partition-events-fn
  (partial part-count-sufficient? 3))

(defn filter-events-fn
  [events]
  (and (>= (count events) 8)
       (every? (partial part-count-sufficient? 3) events)))

(defn average-pitch
  [vertical-moments]
  (let [pitches (map :pitch (flatten vertical-moments))]
    (/ (apply + pitches)
       (count pitches))))

(defn distinct-by
  "Returns a lazy sequence of the elements of coll, removing any elements that
    return duplicate values when passed to a function f."
  [f coll]
  (let [step (fn step [xs seen]
               (lazy-seq
                ((fn [[x :as xs] seen]
                   (when-let [s (seq xs)]
                     (let [fx (f x)]
                       (if (contains? seen fx)
                         (recur (rest s) seen)
                         (cons x (step (rest s) (conj seen fx)))))))
                 xs seen)))]
    (step coll #{})))

(defn pitch-profile
  [vertical-moments]
  (map (fn [x] (map :pitch x)) vertical-moments))

(require '[progressbar.core :refer [progressbar]])
;; user> (doall (map identity (progressbar (range 10) :print-every 2)))

(defn calculate-sequences [chord-seqs]
  (let [states (map initial-state chord-seqs)]
  (->>
       (map compose-event-seq (progressbar (into [] states)))

       ((fn [x]
          (do (println "\nNumber of generated phrases:" (count x))
              x)))

       (mapcat (fn [x]
                 (->> x
                      (partition-by partition-events-fn)
                      (filter filter-events-fn)
                      (take 1)
                      )))

       ((fn [x]
          (do (println "Number of items before uniquify:" (count x))
              x)))

       (distinct-by pitch-profile)

       ((fn [x]
          (do (println "Number of items after uniquify:" (count x))
              x)))

       (sort-by (fn [x] (let [pitches (map :pitch (first x))]
                          (/ (apply + pitches) (count pitches)))))

       (take 10)
       )))

(defn new-session
  [name sources]
  (spit
   (str "/Users/fred/projects/music/compositions/2015/organ/analysis/" name ".edn")
   (prn-str (calculate-sequences (chord-seqs sources)))))

(def sessions
  {
   "testing" materials
   "testing-2" materials-2
   })

(defn calc-all-sessions
  []
  (map (fn [[k v]]
         (new-session k v))
       sessions))

(defn compose []
  (let
      [data
       (concat
        (clojure.edn/read-string (slurp (str "/Users/fred/projects/music/compositions/2015/organ/analysis/" "testing-2" ".edn")))
        )
       ]
    (map (partial compose-parts
                  [measures/measure-4]
                  200
                  [:upper :lower :ped])
         data)
    ))


;; (calc-all-sessions)
;; (compose)


;; (utils/read-json-file
;;  (str "/Users/fred/projects/music/compositions/2015/organ/analysis/" "testing" ".json"))
;; (file-seq (clojure.java.io/file "../../analysis"))


;; (count
;; (compose (calculate-sequences)))

;; most expensive functions: handle-dissonance, chord-seq/merge-horizontally

;; (->>
;;  (compose)
;;  (count))


;; (time
;;  (->> (compose)
;;       (count))
;;  )


;; (compose)

;; measures tempo part-names
;; (compose)
;; (partition-by #(>= % 3)
;;               [0 1 3 3 2 4 3 3 3 1 2 4 2 3 3 3 2])

;; (defn asdf
;;   [pitches]
;;   {:pitch (->> [0 1 2 3 4 5 6]
;;                (utils/transpose 10)
;;                (map (fn [x] [x])))
;;    :part [:upper]
;;    :fn utils/make-chord-from-pitch-vector-params
;;    :partition (partial utils/cyclic-partition [2])
;;    :max-part-count [1]
;;    :merge-left? [true]
;;    :merge-right? [true]
;;    :duration [1/4]})

;; (asdf 1234)
