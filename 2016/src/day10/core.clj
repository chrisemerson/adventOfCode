(ns day10.core
  (:gen-class)
  (:require [clojure.string :as str]))

(defn get-input
  [filename]
  (str/split-lines (str/trim (slurp filename))))

(defn create-instructions
  [[movements botstate] instruction]
    (let
      [lowhigh (re-matcher #"bot (\d+) gives low to ((?:bot|output) \d+) and high to ((?:bot|output) \d+)" instruction)
       input (re-matcher #"value (\d+) goes to bot (\d+)" instruction)]
      (if (re-find lowhigh)
        (let
          [lharg (partial nth (re-groups lowhigh))]
          [(assoc movements (lharg 1) (hash-map :low (lharg 2) :high (lharg 3))) botstate])
        (if (re-find input)
          (let
            [iarg (partial nth (re-groups input))]
            [movements (assoc botstate (iarg 2) (conj (get botstate (iarg 2)) (Integer. (iarg 1))))])
          [movements botstate]))))

(defn pass-chip
  [botstate outputs dest value]
  (let
    [matcher (re-matcher #"(bot|output) (\d+)" dest)]
    (if (re-find matcher)
      (let
        [arg (partial nth (re-groups matcher))]
        (if (= "bot" (arg 1))
          [(assoc botstate (arg 2) (conj (get botstate (arg 2)) value)) outputs]
          [botstate (assoc outputs (arg 2) (conj (get outputs (arg 2)) value))]))
      [botstate outputs])))

(defn distribute
  [[botstate movements outputs] bot]
  (let
    [lowdest (:low (get movements bot))
     highdest (:high (get movements bot))
     low (apply min (get botstate bot))
     high (apply max (get botstate bot))
     botstatenochips (assoc botstate bot '())
     [botstatelowpassed outputslowpassed] (pass-chip botstatenochips outputs lowdest low)
     [botstatebothpassed outputsbothpassed] (pass-chip botstatelowpassed outputslowpassed highdest high)]
    [botstatebothpassed movements outputsbothpassed]))

(defn step
  [[botstate movements outputs]]
  (let
    [botswith2chips (filter #(= 2 (count (second %))) botstate)]
    (reduce distribute [botstate movements outputs] (map first botswith2chips))))

(defn bot-comparing-these-chips
  [botstate low high]
  (let
    [containslow (filter (fn [x] (some #(= low %) (second x))) botstate)
     containshigh (filter (fn [x] (some #(= high %) (second x))) containslow)]
    (if (not (empty? containshigh))
      (do
        (println "Bot" (first (first containshigh)) "is responsible for comparing" low "and" high)
        true)
      false)))

(defn all-processing-done
  [botstate]
  (let
    [has-chips (filter #(not (empty? (second %))) botstate)]
    (empty? has-chips)))

(defn -main
  [& args]
  (let
    [instructions (get-input (first args))
     [movements botstate] (reduce create-instructions [{} {}] instructions)]
    (println (nth (first (drop-while #(not (all-processing-done (first %))) (iterate step [botstate movements {}]))) 2))))
