(ns melos.tools.make-note)

(defn note-default
  []
  {:pitch 10,
   :delta-dur 1/4
   :duration 1/4
   :group (gensym "G__")
   :merge-left? false
   :merge-right? false
   :dissonance-contributor? true
   :allow-extension? true
   :part nil,
   :color nil,
   :count 0,
   :onset 0
   :intensity 1})

(defn make-note-
  "Make a representation of a musical note."
  [params]
  (merge (note-default) params))
  ;; [& {:keys [pitch onset color delta-dur duration group
  ;;            desired-sustain part count merge-left? merge-right?
  ;;            dissonance-contributor?
  ;;            allow-extension?]
  ;;     :or {pitch 10,
  ;;          delta-dur 1/4
  ;;          duration 1/4
  ;;          desired-sustain nil,
  ;;          group nil
  ;;          merge-left? false
  ;;          merge-right? false
  ;;          dissonance-contributor? true
  ;;          allow-extension? true
  ;;          part nil,
  ;;          color nil,
  ;;          count 0,
  ;;          onset 0
  ;;          intensity 1}}]
  ;; {:pitch pitch
  ;;  :delta-dur delta-dur
  ;;  :duration duration
  ;;  :dissonance-contributor? dissonance-contributor?
  ;;  :allow-extension? allow-extension?
  ;;  :color color
  ;;  :merge-left? merge-left?
  ;;  :merge-right? merge-right?
  ;;  :group group
  ;;  :desired-sustain desired-sustain
  ;;  :onset onset
  ;;  :part part
  ;;  :count count})

(def make-note
  (memoize make-note-))
