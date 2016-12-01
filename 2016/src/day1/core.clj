(ns day1.core
  (:gen-class)
  (:require [clojure.string :as str]))

(defn import-instructions
  [file]
  (str/split (str/trim (slurp file)) #",\s+"))

(defn get-new-direction
  [currentdir instruction]
  (let
    [directions [:N :E :S :W]
     curindex (.indexOf directions currentdir)]
    (case instruction
      "L" (nth directions (mod (dec curindex) 4))
      "R" (nth directions (mod (inc curindex) 4)))))

(defn process-instruction
  [[x y d] instruction]
  (let
    [[_ dirinst distinst] (re-matches #"^([DLRU])(\d+)$" instruction)
     newdir (get-new-direction d dirinst)]
     (case newdir
       :N (vector x (+ y (Integer. distinst)) newdir)
       :W (vector (- x (Integer. distinst)) y newdir)
       :E (vector (+ x (Integer. distinst)) y newdir)
       :S (vector x (- y (Integer. distinst)) newdir))))

(defn -main
  [& args]
  (println (reduce + (take 2 (reduce process-instruction [0 0 :N] (import-instructions (first args)))))))
