(ns aoc.core
  (:gen-class)
  (:require day1.core)
  (:require day2.core)
  (:require day3.core)
  (:require day4.core)
  (:require day5.core)
  (:require day6.core)
  (:require day7.core)
  (:require day8.core))

(defn -main
  [day & args]
  (let [ns (symbol (str "day" day ".core"))]
    (apply (ns-resolve ns (symbol "-main")) args)))
