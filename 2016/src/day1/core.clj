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

(defn add-visited-location
  [visitedlocations x y]
  (conj visitedlocations [x y]))

(defn move
  [x y direction distance]
  (case direction
    :N (vector x (+ y distance))
    :W (vector (- x distance) y)
    :E (vector (+ x distance) y)
    :S (vector x (- y distance))))

(defn move-iterable
  [[x y direction visitedlocations]]
  (let
    [[x y] (move x y direction 1)]
    (vector x y direction (add-visited-location visitedlocations x y))))

(defn parse-instruction
  [instruction]
  (let
    [[_ direction distance] (re-matches #"^([DLRU])(\d+)$" instruction)]
    (vector direction (Integer. distance))))

(defn process-instruction
  [[x y d visitedlocations] instruction]
  (let
    [[dirinst distinst] (parse-instruction instruction)
     newdir (get-new-direction d dirinst)
     [newx newy _ visitedlocations] (nth (iterate move-iterable [x y newdir visitedlocations]) distinst)]
     (vector newx newy newdir visitedlocations)))

(defn in?
  [coll el]
  (some #(= el %) coll))

(defn -main
  [& args]
  (let
    [[x y d visitedlocations] (reduce process-instruction [0 0 :N [[0 0]]] (import-instructions (first args)))
     visitedtwice (map (fn [item] (first item)) (filter #(> (second %) 1) (frequencies visitedlocations)))]
    (println
      "Easter Bunny HQ is"
      (+ (Math/abs x) (Math/abs y))
      "blocks away (pt 1) and"
      (reduce (fn [a b] (+ (Math/abs a) (Math/abs b))) (first (drop-while #(not (in? visitedtwice %)) visitedlocations)))
      "blocks away (pt 2)")))
