(ns melos.tools.measures)

(def measure-1
  {:w-duration [5 4]
   ;; :top-level true
   :duration [4 4]
   :children [
              {:w-duration [4 4]
               :duration [3 4]
               :children [
                          {:w-duration [2 4]
                           :duration [2 4]
                           :children [
                                      {:w-duration [1 4]
                                       :duration [1 4]
                                       :event nil
                                       :children nil}
                                      {:w-duration [1 4]
                                       :duration [1 4]
                                       :event nil
                                       :children nil}
                                      ]}
                          {:w-duration [2 4]
                           :duration [2 4]
                           :children [
                                      {:w-duration [1 4]
                                       :duration [1 4]
                                       :event nil
                                       :children nil}
                                      {:w-duration [1 4]
                                       :duration [1 4]
                                       :event nil
                                       :children nil}
                                      ]}
                          ]}
              {:w-duration [2 4]
               :duration [2 4]
               :children [
                          {:w-duration [1 4]
                           :duration [1 4]
                           :event nil
                           :children nil}
                          {:w-duration [1 4]
                           :duration [1 4]
                           :event nil
                           :children nil}
                          ]}
              ]})

(def measure-2
  {:w-duration [5 4]
   :duration [4 4]
   :children [
              {:w-duration [2 4]
               :duration [2 4]
               :children [
                          {:w-duration [1 4]
                           :duration [1 4]
                           :event nil
                           :children nil}
                          {:w-duration [1 4]
                           :duration [1 4]
                           :event nil
                           :children nil}
                          ]}
              {:w-duration [2 4]
               :duration [2 4]
               :children [
                          {:w-duration [1 4]
                           :duration [1 4]
                           :event nil
                           :children nil}
                          {:w-duration [1 4]
                           :duration [1 4]
                           :event nil
                           :children nil}
                          ]}
              ]})
