(ns day8.core
  (:gen-class)
  (:require [clojure.string :as str]))

(defn get-input
  [filename]
  (str/split-lines (str/trim (slurp filename))))

(def emptyboard (repeat 6 (repeat 50 ".")))

(defn rect
  [board w h]
  (concat
    (map #(concat (repeat w "#") (drop w %)) (take h board))
    (drop h board)
  ))

(defn rotate-row
  [board row n]
  (concat
    (take row board)
    (list (concat (take-last n (nth board row)) (drop-last n (nth board row))))
    (drop (inc row) board)))

(defn rotate-col
  [board col n]
  (apply map list (rotate-row (apply map list board) col n)))

(defn count-pixels
  [board]
  (reduce + (map (fn [x] (reduce + (map #(case % "." 0 "#" 1) x))) board)))

(defn process-instruction
  [board instruction]
  (let
    [rect-matcher (re-matcher #"^rect (\d+)x(\d+)$" instruction)
     rotate-col-matcher (re-matcher #"^rotate column x=(\d+) by (\d+)$" instruction)
     rotate-row-matcher (re-matcher #"^rotate row y=(\d+) by (\d+)$" instruction)]
    (if (re-find rect-matcher)
      (let
        [re-groups rect-matcher]
        (rect board (Integer. (nth re-groups 1)) (Integer. (nth re-groups 2))))
      (if (re-find rotate-col-matcher)
        (let
          [re-groups rotate-col-matcher]
          (rotate-col board (Integer. (nth re-groups 1)) (Integer. (nth re-groups 2))))
        (if (re-find rotate-row-matcher)
          (let
            [re-groups rotate-row-matcher]
            (rotate-row board (Integer. (nth re-groups 1)) (Integer. (nth re-groups 2))))
          board)))))

(defn -main
  [& args]
  (let
    [result (reduce process-instruction emptyboard (get-input (first args)))]
    (println (count-pixels result))
    (doseq
      [x result]
      (println x))))
