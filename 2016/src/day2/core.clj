(ns day2.core
  (:gen-class)
  (:require [clojure.string :as str]))

(defn get-instructions
  [filename]
  (str/split-lines (str/trim (slurp filename))))

(defn move
  [[x y] instruction]
  (case instruction
    \U (vector x (max 0 (dec y)))
    \D (vector x (min 2 (inc y)))
    \L (vector (max 0 (dec x)) y)
    \R (vector (min 2 (inc x)) y)
    (vector x y)))

(defn return-number
  [[x y]]
  (let
    [keypad [[1 2 3] [4 5 6] [7 8 9]]]
    ((keypad y) x)))

(defn find-number
  [instructions startingposition]
  (let
    [headpos (vector (reduce move startingposition (first instructions)))]
    (if (> (count instructions) 1)
      (concat
        headpos
        (find-number (rest instructions) (headpos 0)))
      headpos)))

(defn -main
  [& args]
  (println (apply str (map return-number (find-number (get-instructions (first args)) [1 1])))))
