;; (ns melos.score.main
;;   (:require [clojure.java.shell :refer [sh]]
;;             [schema.core :as s]
;;             [melos.lib
;;              [chord :as chord]
;;              [chord-seq :as chord-seq]
;;              [note :as note]
;;              [part :as part]
;;              [rhythm-tree :as rhythm-tree]
;;              [schemas :as ms]
;;              [measure :as measure-util]
;;              [utils :as utils]]))

;; ;; There is a lot of schema validation in the lib namespaces.
;; ;; Setting this to `false` significantly improves performance.
;; (s/set-fn-validation! false)

;; ;; basic 4/4 subdivisions
;; (def measure:4-4
;;   (measure-util/parse-rtm-tree-node
;;    (measure-util/stretch-tree [4 4] 0 [[0] [0] [0] [0]])))

;; (def measure:stretched
;;   (measure-util/parse-rtm-tree-node
;;    (measure-util/stretch-tree [4 4] 1 [[1 0] [0 0] [0] [0] [0] [0] [0]])))

;; ;; (let [pitch-groups [0 :end [0 1] :end [1] [1] [1] :end [0] [0 2] :end [0 2 4]]

;; (defn chromatic
;;   [part transposition]
;;   (let [pitch-groups [0 :end [1 8] :end [1 8] :end [8] :end 2 :end 3 :end 4 :end]
;;         pitches (->> pitch-groups
;;                      (filter (complement keyword?)))
;;         segmentation (->> pitch-groups utils/segment-melody (map count))]
;;     (->> {:pitch (->> pitches
;;                       (map (fn [x] (if (number? x) [x] x)))
;;                       (utils/transpose-all transposition))
;;           :part [part]
;;           ;; :is-rest? [false true false true false false true false true false]
;;           :is-rest? [false]
;;           ;; :group [0 0 0 1 2 3 3 3]
;;           :merge-left? [false]
;;           :merge-right? [false]
;;           :notation [{:registration "A"} {:registration "A"}]
;;           :duration [1/4 1/8 1/8]}
;;          utils/unfold-parameters
;;          (map utils/make-chord-from-pitch-vector-params)
;;          cycle
;;          (utils/cyclic-partition segmentation)
;;          (map #(s/validate ms/Phrase %)))))

;; (defn compose-section
;;   [part-names measures]
;;   ;; TODO: segment sections according to which criteria?
;;   {:tempo 144
;;    :measures (mapcat (fn [part-name] (->> measures
;;                                           chord-seq/merge-horizontally
;;                                           (rhythm-tree/extend-last 4/4)
;;                                           (rhythm-tree/make-r-tree [measure:stretched])
;;                                           (part/split-out-part part-name)
;;                                           (rhythm-tree/merge-all-tied)
;;                                           ))
;;                      part-names)})

;; (require '[clojure.data.json :as json])

;; (defn make-score
;;   [output-path]
;;   (let [melody-sources (atom {:voice-1 (chromatic :voice-1 5)
;;                               ;; :voice-2 (chromatic :voice-2 2)
;;                               :voice-3 (chromatic :voice-3 -2)
;;                               :voice-4 (chromatic :voice-4 -7)
;;                               :voice-5 (chromatic :voice-5 -16)})
;;         diss-fn-params {:max-count 100
;;                         :max-lingering 300
;;                         :diss-params [0 1 2]}
;;         melodic-indices (->> [:voice-3
;;                               :voice-1
;;                               :voice-3
;;                               :voice-1
;;                               :voice-3
;;                               :voice-3
;;                               :voice-5
;;                               :voice-1
;;                               :voice-3
;;                               :voice-5]
;;                              (cycle)
;;                              (take 50))
;;         final-event-extensions [4/4 4/4]
;;         section-markup-data [{:registration {:voice-1 "a"
;;                                              :voice-2 "a"
;;                                              :voice-3 "b"
;;                                              :voice-4 "b"
;;                                              :voice-5 "a"}},
;;                              {:registration {:voice-1 "b"
;;                                              :voice-2 "a"
;;                                              :voice-3 "b"
;;                                              :voice-4 "b"
;;                                              :voice-5 "b"}}]
;;         tempi [90 130]]
;;     (->> (chord-seq/collect-events-in-segment
;;           melodic-indices
;;           melody-sources)
;;          ;; Extend phrases
;;          (chord-seq/extend-phrases diss-fn-params [])

;;          ;; Filter phrases

;;          ;; TODO: stack overflow error when partition too large
;;          ;; (utils/partition-groups (comp :phrase-end first) [] [])
;;          (partition 40)

;;          ;; Compose part

;;          first
;;          ;; (mapcat (fn [x] (->> x
;;          ;;                      chord-seq/merge-horizontally
;;          ;;                      (rhythm-tree/extend-last 4/4)
;;          ;;                      (rhythm-tree/make-r-tree [measure:4-4]))))
;;                            ;; (#(assoc % :part
;;                       ;; (keys)
;;                       ;; clojure.pprint/pprint

;;          ;; TODO: segmentation?
;;          ;; {:tempo 123 :measures [] :fns ["function_name"]}

;;          (compose-section [:voice-1])
;;          ((fn [x] [x]))

;;          ;; (#(assoc % :part-name :voice-1))
;;          ;; keys
;;          ;; (map :measure-root)

;;          ;; (#(part/split-out-part % :voice-1))
;;          ;; (#(mapcat (fn [x] (->> x
;;          ;;                        (part/split-out-part %)
;;          ;;                        (map (fn [part-name] (assoc % :part part-name)))))
;;          ;;        [:voice-1 :voice-3 :voice-5]))
;;          ;; (utils/export-to-json output-path)

;;          ;; (map (fn [section-name tempo section-markup extension phrase]
;;          ;;        (->> phrase
;;          ;;             chord-seq/merge-horizontally
;;          ;;             (rhythm-tree/extend-last extension)
;;          ;;             (rhythm-tree/make-r-tree [measure:stretched])
;;          ;;             (part/compose-section
;;          ;;              section-name
;;          ;;              tempo
;;          ;;              [:voice-1
;;          ;;               ;; :voice-2
;;          ;;               :voice-3
;;          ;;               ;; :voice-4
;;          ;;               :voice-5]
;;          ;;              section-markup)))
;;          ;;      (range)
;;          ;;      (cycle tempi)
;;          ;;      (cycle section-markup-data)
;;          ;;      (cycle final-event-extensions))
;;          ;; (utils/export-to-json output-path))))
;;          )))

;; ;; (require '[clojure.tools.namespace.repl :refer [refresh]])

;; (defn render []
;;   (let [pdf-output-path "../../output/score.pdf"
;;         python "../../env/bin/python"
;;         script "../../py/main.py"
;;         music (make-score "oijoijoij")
;;         score {:title "Test"
;;                :author "Anonymous"
;;                :music music}
;;         result (sh python script "--output" "oij" :in (json/write-str score))]
;;     (if (= (:exit result) 1)
;;       (print (:err result))
;;       (print (:out result)))
;;     ))


;; ;; lksjdf


;; ;; TODO: shelling out?
