(ns day9.core
  (:gen-class)
  (:require [clojure.string :as str]))

(defn get-input
  [filename]
  (str/trim (slurp filename)))

(defn get-decoded-part
  [string start length n]
  (apply str (repeat n (subs string start (+ start length)))))

(defn decode-string
  [string]
  (loop
    [origstring string
     decodedstring ""]
     (let
       [matcher (re-matcher #"^\((\d+)x(\d+)\)" origstring)]
       (if (empty? origstring)
         decodedstring
         (if (re-find matcher)
           (let
             [matches (re-groups matcher)]
             (recur
               (subs origstring (+ (count (first matches)) (Integer. (second matches))))
               (str decodedstring (get-decoded-part origstring (count (first matches)) (Integer. (second matches)) (Integer. (nth matches 2))))))
           (recur
             (subs origstring 1)
             (str decodedstring (subs origstring 0 1))))))))

(defn -main
  [& args]
  (println (count (decode-string (get-input (first args))))))
