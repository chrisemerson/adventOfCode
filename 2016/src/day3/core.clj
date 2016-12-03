(ns day3.core
  (:gen-class)
  (:require [clojure.string :as str]))

(defn get-instructions
  [filename]
  (str/split-lines (str/trim (slurp filename))))

(defn triangle?
  [x y z]
  ())

(defn getvector
  [line]
  (str/split line #"\s+"))

(defn -main
  [& args]
  (println (map getvector (get-instructions (first args)))))
