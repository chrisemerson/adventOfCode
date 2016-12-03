(ns day3.core
  (:gen-class)
  (:require [clojure.string :as str]))

(defn cleanline
  [line]
  (map (fn [x] (Integer. x)) (filter (fn [x] (not (empty? x))) (map str/trim (str/split line #"\s+")))))

(defn get-instructions
  [filename]
  (map cleanline (str/split-lines (str/trim (slurp filename)))))

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
  (println (count (filter triangle? (get-instructions (first args))))))
