(ns day3.core
  (:gen-class)
  (:require [clojure.string :as str]))

(defn transpose-lines
  [[a b c] [d e f] [g h i]]
  (vector (vector a d g) (vector b e h) (vector c f i)))

(defn rearrange-lines
  [input transposed]
  (if (empty? input) transposed (rearrange-lines (drop 3 input) (concat transposed (apply transpose-lines (take 3 input))))))

(defn cleanline
  [line]
  (map (fn [x] (Integer. x)) (filter (fn [x] (not (empty? x))) (map str/trim (str/split line #"\s+")))))

(defn get-input
  [filename]
  (rearrange-lines (map cleanline (str/split-lines (str/trim (slurp filename)))) '()))

(defn triangle?
  [[x y z]]
  (let
    [longestside (max x y z)]
    (< longestside (- (+ x y z) longestside))))

(defn getvector
  [line]
  (str/split line #"\s+"))

(defn -main
  [& args]
  (println (count (filter triangle? (get-input (first args))))))
