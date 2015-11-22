(ns melos.score.score-utils)

(defn parts-in-chord
  [chord]
  (set (map :part chord)))

(defn part-count-sufficient?
  [minimum chord]
  (let [part-count ((comp count parts-in-chord) chord)]
    (>= part-count minimum)))

(defn pitch-profile
  [vertical-moments]
  (map (fn [x] (map :pitch x)) vertical-moments))
