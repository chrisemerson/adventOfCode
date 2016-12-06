(ns day6.core
  (:gen-class)
  (:require [clojure.string :as str]))

(defn get-input
  [filename]
  (str/split-lines (str/trim (slurp filename))))

(defn find-message
  [lines]
  (apply str (map (comp first first #(sort-by second > %)) (map frequencies (apply map vector lines)))))

(defn -main
  [& args]
  (println (find-message (get-input (first args)))))
