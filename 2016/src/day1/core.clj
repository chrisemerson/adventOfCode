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
  [[x y direction visitedlocations]]
  (case direction
    :N (vector x (inc y) direction (add-visited-location visitedlocations x (inc y)))
    :W (vector (dec x) y direction (add-visited-location visitedlocations (dec x) y))
    :E (vector (inc x) y direction (add-visited-location visitedlocations (inc x) y))
    :S (vector x (dec y) direction (add-visited-location visitedlocations x (dec y)))))

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
     [newx newy _ visitedlocations] (nth (iterate move [x y newdir visitedlocations]) distinst)]
     (vector newx newy newdir visitedlocations)))

(defn in?
  [coll el]
  (some #(= el %) coll))

(defn firstrepeateditem
  [coll]
  (let
    [appearstwice (map (fn [item] (first item)) (filter #(> (second %) 1) (frequencies coll)))]
    (first (drop-while #(not (in? appearstwice %)) coll))))

(defn +abs
  [& args]
  (apply + (map #(Math/abs %) args)))

(defn -main
  [& args]
  (let
    [startingvector [0 0 :N [[0 0]]]
     instructions (import-instructions (first args))
     [x y d visitedlocations] (reduce process-instruction startingvector instructions)]
    (println
      "Easter Bunny HQ is"
      (+abs x y)
      "blocks away (pt 1) and"
      (apply +abs (firstrepeateditem visitedlocations))
      "blocks away (pt 2)")))
