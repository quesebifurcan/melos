(ns melos.lib.utils
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.walk :as walk]
            [clojure.algo.generic.functor :as functor]
            [melos.lib
             [note :refer [make-note]]
             [schemas :as ms]]
            [schema.core :as s]))

(defn abs [n] (max n (- n)))

(defn sum [args] (apply + args))

(defn rotate
  ([s] (rotate 1 s))
  ([n s] (lazy-cat (drop n s) (take n s))))

(letfn [(merge-in* [a b]
          (if (map? a)
            (merge-with merge-in* a b)
            b))]
  (defn merge-in
    [& args]
    (reduce merge-in* nil args)))

(defn- triangular*
  ([] (triangular* 0 1))
  ([sum n]
   (let [new-sum (+ sum n)]
     (cons new-sum (lazy-seq (triangular* new-sum (inc n)))))))

(def triangular (triangular*))

(defn triangular-n
  "Get the nth triangular number."
  [n]
  (last (take n triangular)))

(defn read-json-file
  "Read a json file and make sure that any key which is a string is
  transformed into a keyword."
  [f]
  (with-open [reader (clojure.java.io/reader f)]
    (->> (json/read reader)
         (walk/keywordize-keys))))

(defn write-json-to-file [path data]
  (spit path (json/write-str data)))

(defn export-to-json
  "Export a score. Replace all duration ratios with non-reduced ratio vectors."
  [path score]
  (->> score
       ;; Replace ratios with vector ratios for better json.
       ;; (walk/postwalk (fn [form]
       ;;                  (if (get form :duration)
       ;;                    (update-in form
       ;;                               [:duration]
       ;;                               ratio->non-reduced-ratio-vector)
       ;;                    form)))
       (write-json-to-file path)))

(defn mapply
  [f & args]
  (apply f (apply concat (butlast args) (last args))))

(defn maybe-vec [x] (if (number? x) [x] x))

(defn make-chord-from-pitch-vector-params
  [{:keys [pitch] :as m}]
  (let [group (gensym "G__")]
    (map (fn [p]
           (make-note (merge {:pitch p :group group}
                             (dissoc m :pitch))))
         pitch)))

(defn make-chord-from-pitch-vector-params-2
  [{:keys [pitch] :as m}]
  (map (fn [p]
         (let [group (gensym "G__")]
           (make-note (merge {:pitch p :group group}
                             (dissoc m :pitch)))))
         pitch))

(defn parse-params
  [x]
  (if (list? x)
    (repeat (last x) (first x))
    [x]))

(defn cyclic-partition
  [splits xs]
  (cons (take (first splits) xs)
        (lazy-seq (cyclic-partition (rotate splits)
                                    (drop (first splits) xs)))))

(defn cyclic-repeats
  [repeats xs]
  (if (seq xs)
    (concat (repeat (first repeats) (first xs))
            (lazy-seq (cyclic-repeats (rotate repeats)
                                      (rotate xs))))))

(defn combine-partitions
  [& partitions]
  (let [cycle-len (apply * (map (fn [x] (apply + x))
                                partitions))
        seqs (map (fn [part]
                    (take-while (fn [x] (< x cycle-len))
                                (reductions + 0 (cycle part))))
                  partitions)]
    (->> (sort (set (apply concat seqs)))
         (partition 2 1)
         (map (fn [[x y]] (- y x))))))

;; TODO: move to scores/
(defn unfold-events
  [m]
  (let [f (:fn m)
        partition-fn (:partition m)
        drop-n (get m :drop-n 0)
        m (dissoc m :fn :partition :drop-n)]
  (->> (map (fn [x] x)
            (vals m))
       (map cycle)
       (apply map vector)
       (map (fn [x] (zipmap (keys m) x)))
       (map f)
       (partition-fn)
       (drop drop-n)
       (map #(s/validate [ms/Chord] %)))))

(defn valid-melodic-indices?
  [indices source]
  (every? (fn [i] (contains? @source i)) indices))

(defn make-score-segment
  [{:keys [melodic-indices melody-sources] :as m}]
  (assert (valid-melodic-indices? melodic-indices
                                  melody-sources))
  m)

(defn unfold-range
  [[start stop]]
  (let [direction (if (> start stop) -1 1)]
    (range start stop direction)))

(defn unfold-ranges
  [& ranges]
  (mapcat unfold-range ranges))

(defn partition-and-interleave-phrases
  [& colls]
  (let [seqs (map (fn [[partitioning coll]]
                    (cyclic-partition partitioning (cycle coll)))
                  colls)]
    (->> (apply interleave seqs)
         (flatten))))

(defn gradually-expand-chord
  [pitches]
  (rest (reductions conj [] pitches)))

(defn gradually-expand-chords
  [& chords]
  (mapcat gradually-expand-chord chords))

(defn transpose
  [step coll]
  (map (partial + step) coll))

(defn update-state
  [initial-state updates]
  (reduce (fn [m [k v]]
            (update-in m k (fn [_] v)))
          initial-state
          updates))

(defn lindenmayer
  "A simple lindenmayer machine. Borrowed from
  https://brehaut.net/blog/2011/l_systems."
  [rule depth s]
  (if (zero? depth) s
      (mapcat #(lindenmayer rule (dec depth) (rule % [%])) s)))

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

;; Rhythm Trees

(defn ratio-to-non-reduced-ratio-vector
  ;; "Since it is not possible to encode ratios as json, we need to
  ;; convert all ratios to a non-reduced vector of numbers."
  [r]
  (let [r (clojure.lang.Numbers/toRatio (rationalize r))]
    ((juxt numerator denominator) r)))

(defn ratio-calc
  [f args]
  (apply f (map (fn [[num denom]] (/ num denom)) args)))

(def rv+ (partial ratio-calc +))
(def rv- (partial ratio-calc -))

(defn get-child-durations
  [children]
  (->> (map first children)
       (rv+)
       (ratio-to-non-reduced-ratio-vector)))

(defn parse-rtm-tree-node
  [[dur children]]
  (if ((complement nil?) children)
    (let [w-dur (get-child-durations children)]
      {:duration dur
       :w-duration w-dur
       :children (map parse-rtm-tree-node children)})
    {:duration dur
     :w-duration dur
     :children nil
     :event nil}))

(defn partition-groups
  [f curr coll l]
  (if (empty? l)
    (if (empty? curr)
      coll
      (concat coll [curr]))
    (let [nxt (first l)]
      (if (f nxt)
        (partition-groups f
                          []
                          (concat coll
                                  [(concat curr
                                           [nxt])])
                          (rest l))
        (partition-groups f
                          (concat curr [nxt])
                          coll
                          (rest l))))))

(defn transpose-all
  [step forms]
  (clojure.walk/postwalk
   (fn [form]
     (cond (number? form)
           (+ form step)
           :else
           form))
   forms))

(defn unfold-parameters
  [m]
  (->> m
       (iterate (partial functor/fmap rotate))
       (map (partial functor/fmap first))))

;; Combinatorics
(ns melos.score.combinations
  (:require [clojure.math.combinatorics :as combinatorics]))

(defn unfold-parameters
  [m]
  (map (fn [x] (zipmap (keys m) x))
       (apply combinatorics/cartesian-product (vals m))))

(defn- substitute-tagged-elts
  [tag placeholders form]
  (if (and (vector? form)
           (= (first form) tag))
    (let [placeholder (str (gensym "A__"))
          value (second form)]
      (do (swap! placeholders assoc placeholder value)
          placeholder))
    form))

(defn- replace-if-placeholder
  [replacement-map form]
  (if (and (string? form)
           (contains? replacement-map form))
    (get replacement-map form)
    form))

(defn nested-map-product
  [form]
  (let [placeholders (atom {})
        mod-form (clojure.walk/postwalk
                  (partial substitute-tagged-elts 'unfold placeholders)
                  form)]
    (swap! placeholders unfold-parameters)
    (map (fn [replacement-map]
           (clojure.walk/postwalk
            (partial replace-if-placeholder replacement-map)
            mod-form))
         @placeholders)))

;; weave-seqs helper functions

(defn- get-highest-keys
  [m]
  (let [max-val (apply max (vals m))]
    (->> m
         (filter (fn [[k v]] (= v max-val)))
         (keys))))

(defn- get-next-key
  [m exclude]
  (let [candidates (set (get-highest-keys m))]
    (if (> (count candidates) 1)
      (first (clojure.set/difference candidates exclude))
      (first candidates))))

(defn- count-map->key-sequence
  ([m]
   (count-map->key-sequence m #{} []))
  ([m prev coll]
   (if (every? (fn [x] (= 0 x)) (vals m))
     coll
     (let [nxt (get-next-key m prev)]
       (count-map->key-sequence (update m nxt dec)
                                #{}
                                (conj coll nxt))))))

(defn- take-by-keyword-seq
  [m kw-seq]
  (if (empty? kw-seq)
    nil
    (let [curr-key (first kw-seq)]
      (cons (first (get m curr-key))
            (take-by-keyword-seq (update m curr-key (partial drop 1))
                                 (rest kw-seq))))))

(defn weave-seqs
  [m]
  (let [count-map (clojure.algo.generic.functor/fmap count m)
        key-seq (count-map->key-sequence count-map)]
    (take-by-keyword-seq m key-seq)))

;; (let [a {:a (range 7) :b (range 70 72) :c (range 1111 1122)}]
;;   (weave-seqs a))
