(ns aoc.core
  (:gen-class)
  (:require day1.core))

(defn -main
  [day & args]
  (let [ns (symbol (str "day" day ".core"))]
    (apply (ns-resolve ns (symbol "-main")) args)))
