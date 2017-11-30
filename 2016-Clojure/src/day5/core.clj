(ns day5.core
  (:gen-class)
  (:require [clojure.string :as str] [digest]))

(defn fill-position
  [password position character]
  (if (= (subs password position (inc position)) "_")
    (str (subs password 0 position) character (subs password (inc position)))
    password))

(defn fill-positions
  [password hashes]
  (if (re-find #"_" password)
    (let
      [nexthash (first hashes)
       resthashes (rest hashes)
       position (subs nexthash 5 6)
       character (subs nexthash 6 7)]
      (if (some #(= position %) '("0" "1" "2" "3" "4" "5" "6" "7"))
        (fill-positions (fill-position password (Integer. position) character) resthashes)
        (fill-positions password resthashes)))
    password))

(defn -main
  [& args]
  (println
    (->>
      (iterate inc 0)
      (map #(str (first args) %))
      (map digest/md5)
      (filter #(= (subs % 0 5) "00000"))
      (fill-positions "________"))))
