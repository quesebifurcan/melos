(ns melos.lib.utils
(:require [clojure.data.json :as json]
[clojure.java.io :as io]
[clojure.walk :as walk]
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

(s/defn ratio->non-reduced-ratio-vector
  ;; "Since it is not possible to encode ratios as json, we need to
  ;; convert all ratios to a non-reduced vector of numbers."
  :- [s/Int]
  [r]
  (let [r (clojure.lang.Numbers/toRatio (rationalize r))]
    ((juxt numerator denominator) r)))

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
