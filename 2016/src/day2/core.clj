(ns day2.core
  (:gen-class)
  (:require [clojure.string :as str]))

(def keypad [[\X \X 1 \X \X] [\X 2 3 4 \X] [5 6 7 8 9] [\X \A \B \C \X] [\X \X \D \X \X]])

(defn get-instructions
  [filename]
  (str/split-lines (str/trim (slurp filename))))

(defn return-key
  [[x y]]
  ((keypad y) x))

(defn move
  [[x y] instruction]
  (let
    [[newx newy] (case instruction
                   \U (vector x (max 0 (dec y)))
                   \D (vector x (min (dec (count keypad)) (inc y)))
                   \L (vector (max 0 (dec x)) y)
                   \R (vector (min (dec (count keypad)) (inc x)) y)
                   (vector x y))]
    (if (= \X (return-key [newx newy])) [x y] [newx newy])))

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
  (println (apply str (map return-key (find-number (get-instructions (first args)) [(/ (dec (count keypad)) 2) (/ (dec (count keypad)) 2)])))))
