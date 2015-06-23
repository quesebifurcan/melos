(ns melos.tools.default-horizontal-merge)

;; Merge horizontally

(defn can-merge?
  [curr next]
  (let [old-curr (filter #(> (:count %) 0) next)
        news (filter #(= (:count %) 0) next)]
    (and (= (count curr) (count old-curr))
         (every? #(:merge-left? %) news)
         (every? #(:merge-right? %) old-curr))))

(defn merge-elts
  [a b]
  (let [melodic-notes (filter #(= (:count %) 0) b)]
    (concat a melodic-notes)))

(defn maybe-merge
  ([events]
   (if (seq events)
     (maybe-merge (first events)
                  (rest events))))
  ([head events]
   (cond (empty? events)
         (list head)
         (can-merge? head (first events))
         (maybe-merge (merge-elts head
                                  (first events))
                      (rest events))
         :else
         (cons head (maybe-merge events)))))
