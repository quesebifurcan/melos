(ns melos.scores.materials.dissonance-maps
  (:require [clojure.algo.generic.functor :as functor]
            [clojure.math.numeric-tower :as math]))

(def dissonance-map-2
  (let [interval->dissonance
        {0 0,
         1 2,
         2 1,
         3 6,
         4 6,
         5 6,
         6 6}]
    (functor/fmap #(math/expt % 10/9)
                  interval->dissonance)))

(def maps
  {:favor-dissonant dissonance-map-2})

(defn retrieve
  [k]
  (get maps k))
