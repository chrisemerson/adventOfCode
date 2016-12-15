(ns day14.core
  (:gen-class)
  (:require [digest]))

(defn createhash
  [n salt]
  (nth (iterate digest/md5 (str salt n)) 2017))

(def createhash-m (memoize createhash))

(defn containstriple?
  [hash]
  (let
    [matcher (re-matcher #"(.)\1\1" hash)]
    (if (re-find matcher)
      (nth (re-groups matcher) 1)
      false)))

(def containstriple?-m (memoize containstriple?))

(defn contains5inarow?
  [hash char]
  (re-find (re-pattern (str char char char char char)) hash))

(def contains5inarow?-m (memoize contains5inarow?))

(defn indexproduceskey?
  [n salt]
  (let
    [triple (containstriple?-m (createhash-m n salt))]
    (if triple
      (> (->>
        (inc n)
        (iterate inc)
        (take 1000)
        (pmap #(createhash-m % salt))
        (filter #(contains5inarow?-m % triple))
        (count)) 0)
      false)))

(defn -main
  [& args]
  (println (->>
             (iterate inc 0)
             (filter #(indexproduceskey? % (first args)))
             (take 64)
             (last))))
