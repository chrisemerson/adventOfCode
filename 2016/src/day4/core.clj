(ns day4.core
  (:gen-class)
  (:require [clojure.string :as str]))

(defn get-input
  [filename]
  (str/split-lines (str/trim (slurp filename))))

(defn calculate-checksum
  [room]
  (->> room
    (re-seq #"([A-Za-z]+)-")
    (map rest)
    (map #(apply str %))
    (apply str)
    (frequencies)
    (sort-by first)
    (sort-by second >)
    (take 5)
    (map first)
    (apply str)))

(defn valid-room?
  [room]
  (let
    [[_ _ _ given-checksum] (re-matches #"^(?:([a-zA-Z]+)-)+(\d+)\[([a-zA_Z]+)\]$" room)
     calculated-checksum (calculate-checksum room)]
    (= given-checksum calculated-checksum)))

(defn get-sector-id
  [room]
  (let
    [matches (re-matches #"^(?:([a-zA-Z]+)-)+(\d+)\[([a-zA_Z]+)\]$" room)]
    (Integer. (first (take-last 2 matches)))))

(defn -main
  [& args]
  (println (apply + (map get-sector-id (filter valid-room? (get-input (first args)))))))
