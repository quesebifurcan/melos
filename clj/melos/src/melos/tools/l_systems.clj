(ns melos.tools.l-systems)

(defn lindenmayer
  "A simple lindenmayer machine. Borrowed from
  https://brehaut.net/blog/2011/l_systems."
  [rule depth s]
  (if (zero? depth) s
    (mapcat #(lindenmayer rule (dec depth) (rule % [%])) s)))
