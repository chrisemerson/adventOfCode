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

(defn decrypt-character
  [ch sector-id]
  (if (= \- ch)
    ch
    (-> ch
      (int)
      (- 97)
      (+ sector-id)
      (mod 26)
      (+ 97)
      (char))))

(defn decrypt-room-name
  [room]
  (let
    [[_ room-name] (re-matches #"^([a-zA-Z-]+)-(\d+)\[([a-zA_Z]+)\]$" room)
     sector-id (get-sector-id room)]
    (apply str (map #(decrypt-character % sector-id) room-name))))

(defn -main
  [& args]
  (let
    [filtered-rooms-list (filter valid-room? (get-input (first args)))
     north-pole-objects-room (first (filter #(str/includes? (decrypt-room-name %) "north") filtered-rooms-list))]
    (println (apply + (map get-sector-id filtered-rooms-list)))
    (println "Sector: " (get-sector-id north-pole-objects-room) " Room name: " (decrypt-room-name north-pole-objects-room))))
