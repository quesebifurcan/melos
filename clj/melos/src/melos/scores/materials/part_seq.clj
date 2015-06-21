(ns melos.scores.materials.part-seq)

(def part-seq
  "A seq of part names, in this particular case corresponding to three
  organ manuals."
  (cycle [:lower :upper :lower :upper :ped]))

(defn retrieve
  [k cnt]
  (take cnt (get {:a part-seq} k)))

