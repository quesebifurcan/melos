(ns melos.scores.segments.generators
  (:require [schema.core :as s]
            [melos.tools.schemata :as ms]
            [melos.tools.cycle-params :refer [unfold-parameter-cycles]]
            [melos.tools.utils :as utils]
            [melos.scores.compose-segment :refer [compose-segment]]
            [melos.scores.graphs.score-graph :as score-graph]
            [melos.scores.materials.event-seqs :as event-seqs]
            [melos.scores.materials.measures :as measures]
            [melos.scores.materials.dissonance-maps :as dissonance-maps]
            [melos.scores.ctrl-fns.stepwise :as stepwise]
            [melos.scores.ctrl-fns.pairwise :as pairwise]))

(s/defn initial-score-segment
  :- ms/ScoreSegment
  []
  (utils/make-score-segment {:melodic-indices (take 20 (cycle [
                                                               :upper/a
                                                               :lower/a
                                                               :ped/a
                                               ]))
                             :diss-fn-params {:max-count 10
                                              :max-lingering 300
                                              :diss-value [0 2 4 5]}
                             :interval->diss-map dissonance-maps/default
                             :time-signatures [measures/measure-4]
                             ;; :mod-dur-patterns [pairwise/sustain-dissonant-melody]
                             ;; :mod-dur-patterns [stepwise/remove-dissonant-vertical-moments]
                             ;; :mod-dur-patterns [stepwise/dissonance->durations-mapping]
                             ;; :mod-dur-patterns [pairwise/sustain-parts-count-drops]
                             ;; :mod-dur-patterns [stepwise/apply-melodic-pitch-class-mapping]
                             ;; :mod-dur-patterns [stepwise/apply-durations]
                             :mod-dur-patterns [stepwise/sort-continuous]
                             :tempo 240
                             :part-names [:upper :lower :ped]
                             :melody-sources (atom (event-seqs/organ))}))

(defn melodic-indices
  []
  (let [cnts [1000 4000 4000 4000]]
    (map (fn [cnt pattern]
           ;; (take cnt (cycle [
           ;;                   ;; :upper/a :lower/a :upper/a :ped/a :ped/a :lower/a
           ;;                   ;; :upper/a :lower/a :upper/a :ped/a
           ;;                   ;; :upper/a :lower/a :ped/a
           ;;                   ;; :upper/a :lower/a :ped/a :ped/a
           ;;                   ;; :upper/a :lower/a
           ;;                   ;; :upper/a :lower/a
           ;;                   ;; :upper/a :lower/a
           ;;                   ])))
           (take cnt (cycle pattern)))
         cnts
         [
          [:upper/a :lower/a :ped/a]
          [:upper/a :lower/a :upper/a :ped/a]
          [:upper/a :lower/a :upper/a :lower/a :ped/a]
          [:upper/a :lower/a :upper/a :ped/a :lower/a :ped/a]
          ])))

(s/defn changes
  :- [ms/PartialScoreSegment]
  []
  (let [tempo-measure-link [2 2]]
    (unfold-parameter-cycles
     [{:values (melodic-indices)
       :path [:melodic-indices]
       :cycle [1]}
      {:values [[measures/measure-4] [measures/measure-3]]
       :path [:time-signatures]
       :cycle tempo-measure-link}
      {:values [240]
       :path [:tempo]
       :cycle tempo-measure-link}]
     1)))

(defn compose
  []
  (compose-segment {:initial-state (initial-score-segment)
                    :changes (changes)
                    :graph score-graph/lazy-segment-graph}))

;; (defn generate-segment
;;   [segment]
;;   (->> segment
;;        ;; (score-graph/lazy-segment-graph)
;;        ;; (:merged-horizontally)
;;        ))

;; TODO: remove coupling between melody-sources and melodic-indices.

;; (let [init (assoc (initial-score-segment)
;;                   :events
;; (:merged-horizontally
;;  (score-graph/ranking-graph
;;   (initial-score-segment)))

(require '[melos.tools.selector-sequence :refer [collect-events-in-segment]]
         '[melos.tools.utils :as utils]
         '[melos.tools.cycle-params :as cycle-params]
         '[clojure.algo.generic.functor :as functor]
         '[clojure.math.combinatorics :as combinatorics])

(defn calc-upper-part
  [{:keys [part-name transposition pcs]}]
  (let [ranges [
                (range 12)
                ]
        pitches (apply concat ranges)
        partition-limit (map count ranges)
        ;; partitions [3 3 2 3 2 3]
        pitches (event-seqs/partition-by-start-pcs pitches pcs [] [])
        partitions (map count pitches)]
    {:pitch (->> (flatten pitches)
                 (utils/transpose transposition)
                 (map utils/maybe-vec))
     :part [part-name]
     ;; :allow-extension? (map (fn [x]
     ;;                          (contains? pcs (rem (+ 60 x) 12)))
     ;;                        (flatten pitches))
     ;; :allow-extension? (map (partial allow-extension-fn-2 pcs) (flatten pitches))
     :fn utils/make-chord-from-pitch-vector-params
     :partition (partial utils/cyclic-partition partitions)
     :max-part-count [1]
     :duration [1/4]}))

;; (defn unfold-test
;;   [m]
;;   (->> (utils/unfold-events m)
;;        (take 10)
;;        (flatten)
;;        (map :pitch)))

;; (unfold-test (upper {:part-name :ped
;;                      :transposition -3
;;                      :pcs #{0 2 3 5 7 9 10}}))

(defn unfold-parameters
  [m]
  (map (fn [x] (zipmap (keys m) x))
       (apply combinatorics/cartesian-product (vals m))))

(defn evaluate-once
  [{:keys [part-seq lower ped upper]}]
  (let [event-seqs {:lower lower
                    :upper upper
                    :ped ped}
        event-seqs (functor/fmap (comp utils/unfold-events calc-upper-part)
                                 event-seqs)
        ]
    (->> event-seqs
         ;; ((fn [x] {:melody-sources
         ;;           (atom (functor/fmap utils/unfold-events x))
         ;;           :diss-fn-params {:max-count 10
         ;;                            :max-lingering 300
         ;;                            :diss-value [0 2 4 5]}
         ;;           :interval->diss-map dissonance-maps/default
         ;;           :time-signatures [measures/measure-4]
         ;;           :mod-dur-patterns []
         ;;           :tempo 240
         ;;           :part-names [:upper :lower :ped]
         ;;           :melodic-indices
         ;;           (take 20 (:part-seq part-seq))}))
         ;; (score-graph/lazy-segment-graph)
         ;; (:merged-horizontally)
         )))

;; (time
;;  (let [data {:melody-sources {:upper
;;                               {:part-name [:upper]
;;                                :transposition (range 3)
;;                                :pcs [#{0 2 4 7} #{0 2 4 7}]}
;;                               :lower
;;                               {:part-name [:lower]
;;                                :transposition (range 10 13)
;;                                :pcs [#{0 2 4 7} #{0 5 8 10}]}
;;                               :ped
;;                               {:part-name [:ped]
;;                                :transposition (range 10 13)
;;                                :pcs [#{0 2 4 7} #{0 5 8 10}]}}}
;;        paths {
;;               [:melody-sources :upper :part-name] [:upper]
;;               [:melody-sources :upper :transposition] (range 3)
;;               [:melody-sources :upper :pcs] [#{0 2 4 7} #{0 2 4 7}]
;;               [:melody-sources :upper :fn] ["test"]
;;               [:melody-sources :lower :part-name] [:lower]
;;               [:melody-sources :lower :transposition] (range 3)
;;               [:melody-sources :lower :pcs] [#{0 2 4 7} #{0 2 4 7}]
;;               [:melody-sources :lower :fn] ["test"]
;;               [:part-seq] [[:upper :lower :ped]
;;                            [:lower :lower :ped :ped :upper :upper]]
;;               }
;;        ]

;;    (->> paths
;;         (unfold-parameters)
;;         (first)
;;         ((fn [x] (map (fn [[k v]] [v]) x)))
;;         )

;;     ))

;;------------------

(require '[melos.tools.rtm :as rtm])
(require '[melos.tools.filter-parts :as filter-parts])

(defn evaluate-nested-fns
  [state]
  (clojure.walk/postwalk
   (fn [form]
     (if (and (map? form)
              (contains? form :fn)
              (contains? form :params))
       ((:fn form) (:params form))
       form))
   state))

(defn evaluate-once
  [state changes]
  (let [
        state (reduce (fn [m [k v]]
                        (update-in m k (fn [_] v)))
                      state
                      changes)
        ]
    (->> state
         (evaluate-nested-fns)
         (score-graph/lazy-segment-graph)
         (:merged-horizontally)
         )))

(defn upper-part
  [{:keys [transposition part-name]}]
  (utils/unfold-events (event-seqs/upper part-name transposition)))

(defn calc-event-combinations
  [state changes]
  (map (partial evaluate-once state)
       changes))

(let [changes (unfold-parameters
               {[:melody-sources :upper/a :params :transposition] [-10 10 0]})
      state {:melody-sources {:upper/a {:fn upper-part
                                        :params {:transposition -3
                                                 :part-name :upper}}}
             :diss-fn-params {:max-count 10
                              :max-lingering 300
                              :diss-value [0 2 4 5]}
             :interval->diss-map dissonance-maps/default
             :time-signatures [measures/measure-4]
             :mod-dur-patterns []
             :tempo 240
             :part-names [:upper :lower :ped]
             :melodic-indices (take 21 (cycle [:upper/a]))}
      events (->> (calc-event-combinations state changes)
                  ;; filter here.
                  (second))]
  (->> events
       (rtm/extend-last 7/4)
       (rtm/make-r-tree [measures/measure-4])
       ((fn [rhythmic-tree]
          {:tempo 240
           :parts (->> (map
                        (fn [part-name]
                          {:part-name part-name
                           :events (filter-parts/split-out-part rhythmic-tree part-name)})
                        [:upper :lower :ped])
                       (rtm/merge-all-tied))}))
       (conj [])
       (utils/export-to-json
        "/Users/fred/Desktop/score.json")
       ))

;; (reduce
;;  (fn [m k]
;;    (update-in m [k] (fnil inc 0)))
;;  {}
;;    (seq s)))

;; (let [a {[:a :b] 23
;;          [:a :c] 987}]
;;   (reduce (fn [m [k v]]
;;             (update-in m k (fn [_] v)))
;;           {}
;;           a))


;; (defn test-part
;;   [part-name pcs-option transpositions]
;;   (let [ranges [
;;                 (range 12)
;;                 ]
;;         pitches (apply concat ranges)
;;         partition-limit (map count ranges)
;;         pitches (event-seqs/partition-by-start-pcs pitches pcs-option [] [])
;;         partitions (map count pitches)]
;;     (->> {:pitch (->> (flatten pitches)
;;                       (utils/transpose transpositions)
;;                       (map utils/maybe-vec))
;;           :part [part-name]
;;           ;; :allow-extension? (map (fn [x]
;;           ;;                          (contains? pcs (rem (+ 60 x) 12)))
;;           ;;                        (flatten pitches))
;;           ;; :allow-extension? (map (partial allow-extension-fn-2 pcs) (flatten pitches))
;;           :fn utils/make-chord-from-pitch-vector-params
;;           :partition (partial utils/cyclic-partition partitions)
;;           :max-part-count [1]
;;           :duration [1/4]}
;;          (utils/unfold-events))))























;; (->>
;;  (asdf {:a (oij [7 8 9])
;;         :b (oij [1 2 3])
;;         :c 123
;;         :d {:e {:f (oij [4 3 2 3])}}})
;;  (count))

;; (asdf {:melody-sources (atom {})
;;        :diss-fn-params {:max-count 10
;;                         :max-lingering 300
;;                         :diss-value [0 2 4 5]}
;;        :interval->diss-map dissonance-maps/default
;;        :mod-dur-patterns (oij [1 2 3 4])
;;        :tempo (oij [240 200 180])
;;        :part-names [:upper :lower :ped]
;;        :melodic-indices (take 20 [1 2 3])})

;; (let [c (asdf {:part-name :ped
;;                :transposition (oij [2 4])
;;                :pcs #{0 2 4 7}})]
;;   (->> (map calc-upper-part c)

;; (defmacro asdf
;;   [expr]
;;   (let [coll (atom {})]
;;     (let [mod-expr (do
;;                      (clojure.walk/prewalk
;;                       (fn [form]
;;                         (if (and (list? form)
;;                                  (= (first form) 'oij))
;;                           (let [id (str (gensym "A__"))]
;;                             (do
;;                               (swap! coll (fn [x] (assoc x id (second form))))
;;                               id))
;;                           form))
;;                       expr))]
;;       ;; (swap! coll (fn [x] (unfold-parameters x)))
;;       ;; (cons 'list [(fn [y] (map (fn [x]
;;       ;;                            (clojure.walk/prewalk
;;       ;;                             (fn [form]
;;       ;;                               (if (string? form)
;;       ;;                                 (get x form)
;;       ;;                                 form))
;;       ;;                             mod-expr))
;;       ;;                          @coll))]
;;       ;; (cons 'list @coll)
;;       (concat 'mod-expr
;;       )))

;; (macroexpand-1
;; (let [a
;;       (asdf [{:a 1
;;               :b (oij [1 2 3 4])}
;;              (oij [1 2 3])
;;              (oij [1 2 3])
;;              (oij [1 2 3])
;;              (oij [1 2 3])
;;              ;; (oij [1 2 3])
;;              ;; (oij [1 2 3])
;;              ;; (oij [1 2 3])
;;              ;; (oij [1 2 3])
;;              ;; (oij [1 2 3])
;;              ;; (oij [1 2 3])
;;              ;;          (oij [1 2 3])
;;              ;;          (oij [1 2 3])
;;              ;;          (oij [1 2 3])
;;              ;;          (oij [1 2 3])
;;              ;;          (oij [1 2 3])
;;              ;;          (oij [1 2 3])
;;              ;;          (oij [1 2 3])
;;              ;;          (oij [1 2 3])
;;              ;;          (oij [1 2 3])
;;              ;;          (oij [1 2 3])
;;              ;;          (oij [1 2 3])
;;              ;;          (oij [1 2 3])
;;              ])]
;;   a)

;; (time
;;  (let [a
;;        `{:abc 123
;;          :b {:a [(atom 8) (oij ~(range 3)) 2]}
;;          :c {:d (+ 10 (oij [0 1 2]))}
;;          :d [
;;              (oij [0 1 2])
;;              (oij [0 1 2])
;;              (oij [0 1 2])
;;              (oij [0 1 2])
;;              (oij [0 1 2])
;;              (oij [0 1 2])
;;              (oij [0 1 2])
;;              (oij [0 1 2])
;;              ;; (oij ~(range 3))
;;              ;; (oij ~(range 3))
;;              ;; (oij ~(range 3))
;;              ;; (oij ~(range 3))
;;              ;; (oij ~(range 3))
;;              ;; (oij ~(range 3))
;;              ;; (oij ~(range 3))
;;              ;; (oij ~(range 3))
;;              ;; (oij ~(range 20))
;;              ]
;;          }
;;        coll (atom {})
;;        mod-expr (clojure.walk/prewalk
;;                  (fn [form]
;;                    (if (and (seq? form)
;;                             (= (first form) `oij))
;;                      (let [id (str (gensym "A__"))]
;;                        (do
;;                          (swap! coll (fn [x] (assoc x id (second form))))
;;                          id))
;;                      form))
;;                  a)]
;;    (->> (swap! coll (fn [x] (unfold-parameters x)))
;;         ;; (->>
;;    ;;  (map (fn [x]
;;    ;;         (clojure.walk/prewalk
;;    ;;          (fn [form]
;;    ;;            (if (string? form)
;;    ;;              (get x form)
;;    ;;              form))
;;    ;;          mod-expr))
;;    ;;       @coll)
;;     ;; (map eval)

;;         (conj [mod-expr]))
;;     ))

;; (time
;;  (count
;;  (unfold-parameters {
;;                      :a [1 2 3]
;;                      :b [1 2 3]
;;                      :c [1 2 3]
;;                      :d [1 2 3]
;;                      :e [1 2 3]
;;                      :f [1 2 3]
;;                      :g [1 2 3]
;;                      :h [1 2 3]
;;                      :i [1 2 3]
;;                      :j [1 2 3]
;;                      :k [1 2 3]
;;                      :l [1 2 3]
;;                      })))

;; (into {} '{:a 123 :b 987})

;; (time
;;  (count
;;   (combinatorics/cartesian-product
;;    (range 3)
;;    (range 3)
;;    (range 3)
;;    (range 3)
;;    (range 3)
;;    (range 3)
;;    (range 3)
;;    (range 3)
;;    )))

;; `[1 ~(range 10) {:i (+ 10 oij)} 999]

;; (defn wer
;;   [{:keys [lower upper ped]}
;;    ;; (let [melody-sources
;;    ;;       {:upper (oij [#{0 2 4 7} #{0 1 2 3 5}])
;;    ;;        :lower (oij [#{0 2 4 7} #{0 1 2 3 5}])
;;    ;;        :ped (oij [#{0 2 4 7} #{0 1 2 3 5}])
;;    ;;        :lkjdf (oij [#{0 2 4 7} #{0 1 2 3 5}])
;;    ;;        ;; :lower (test-part :lower
;;    ;;        ;;                   (oij [#{0 2 4 7} #{0 1 2 3 5}])
;;    ;;        ;;                   (oij [-10 0 10]))
;;    ;;        ;; :ped (test-part :lower
;;    ;;        ;;                 (oij [#{0 2 4 7} #{0 1 2 3 5}])
;;    ;;        ;;                 (oij [-10 0 10]))}]
;;    ;;        }]
;;    {:melody-sources (atom {
;;                      :lower (test-part lower)
;;                      :upper (test-part upper)
;;                      :ped (test-part ped)
;;                      })
;;     :diss-fn-params {:max-count 10
;;                      :max-lingering 300
;;                      :diss-value [0 2 4 5]}
;;     :interval->diss-map dissonance-maps/default
;;     :mod-dur-patterns {:options [1 2 3 4]}
;;     :tempo tempo
;;     :part-names [:upper :lower :ped]
;;     :melodic-indices (take 20 [1 2 3])}
;;    ))

;; (wer
;;  {:lower {:pcs:options [#{0 2 4 7} #{0 1 2 3 5}]}
;;   :upper {:options [#{0 2 4 7} #{0 1 2 3 5}]}
;;   :ped {:options [#{0 2 4 7} #{0 1 2 3 5}]}

;; (unfold-parameters {:a [2] :b [2 3 4]})

;; (wer)

;; (unfold-parameters
;;  {"A__67890" [1 2 3], "A__67889" [7 8 9]})


;; (upper
;;  {:pcs #{0 7 4 2}, :transposition 0, :part-name :ped})

;; (->> (combinatorics/cartesian-product
;;       (range 10)
;;       (range 100 110)
;;       (range 1000 1010))
;;      (partition 10)
;;      )

;; how limit the calculations?

;; (let [a {:a [1 2 3]
;;          :b [9 8 7]}
;;       b {:a [100 200 300]
;;          :b [11 22 33]}
;;       c {:upper a
;;          :ped b}]
;;   ;; (->> (apply combinatorics/cartesian-product
;;   ;;             (map unfold-parameters (vals c)))
;;   ;;      (first)))
;;   ;; (->> c
;;   ;;      (:upper)
;;   ;;      (unfold-parameters)
;;   ;;      ))
;;   (->> (functor/fmap unfold-parameters c)
;;        (unfold-parameters)
;;        ))


;; TODO: set max number of results to hold on to; drop the ones not needed.
