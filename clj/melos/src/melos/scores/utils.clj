(ns melos.scores.utils)

(defn maybe-vec [x] (if (number? x) [x] x))
