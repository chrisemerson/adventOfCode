(ns day5.core
  (:gen-class)
  (:require [clojure.string :as str] [digest]))

(defn -main
  [& args]
  (println
  (->>
    (iterate inc 0)
    (map #(str (first args) %))
    (map digest/md5)
    (filter #(= (subs % 0 5) "00000"))
    (take 8)
    (map #(subs % 5 6))
    (apply str))))
