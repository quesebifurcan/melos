(ns melos.scores.materials.dissonance-maps
  (:require [clojure.algo.generic.functor :as functor]
            [clojure.math.numeric-tower :as math]))

(def dissonance-map-1
  (let [interval->dissonance
        {0 0,
         1 6,
         2 4,
         3 3,
         4 2,
         5 1,
         6 5}]
    (functor/fmap #(math/expt % 10/9)
                  interval->dissonance)))

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

(def presets
  {:default dissonance-map-1
   :inverted dissonance-map-2})
