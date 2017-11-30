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

(defn get-decoded-string-length
  [string]
  (loop
    [origstring string
     length 0]
     (let
       [matcher (re-matcher #"^\((\d+)x(\d+)\)" origstring)]
       (if (empty? origstring)
         length
         (if (re-find matcher)
           (let
             [matches (re-groups matcher)
              markerlength (count (first matches))
              contentlength (Integer. (nth matches 1))
              content (subs origstring markerlength (+ markerlength contentlength))
              repeat (Integer. (nth matches 2))]
             (recur
               (subs origstring (+ markerlength contentlength))
               (+ length (* repeat (get-decoded-string-length content)))))
           (recur (subs origstring 1) (inc length)))))))

(defn -main
  [& args]
  (println (get-decoded-string-length (get-input (first args)))))
