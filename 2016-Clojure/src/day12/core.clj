(ns day12.core
  (:gen-class)
  (:require [clojure.string :as str]))

(defn get-input
  [filename]
  (str/split-lines (str/trim (slurp filename))))

(defn handle-cpy
  [a b c d instruction]
  (let
    [source (second (str/split instruction #" "))
     value (case source "a" a "b" b "c" c "d" d (Integer. source))
     dest (nth (str/split instruction #" ") 2)]
    (case dest
      "a" [value b c d]
      "b" [a value c d]
      "c" [a b value d]
      "d" [a b c value]
      [a b c d])))

(defn handle-inc
  [a b c d instruction]
  (case (second (str/split instruction #" "))
    "a" [(inc a) b c d]
    "b" [a (inc b) c d]
    "c" [a b (inc c) d]
    "d" [a b c (inc d)]))

(defn handle-dec
  [a b c d instruction]
  (case (second (str/split instruction #" "))
    "a" [(dec a) b c d]
    "b" [a (dec b) c d]
    "c" [a b (dec c) d]
    "d" [a b c (dec d)]))

(defn handle-jnz
  [a b c d instruction line]
  (let
    [register (second (str/split instruction #" "))
     regval (case register "a" a "b" b "c" c "d" d (Integer. register))
     linestojump (Integer. (nth (str/split instruction #" ") 2))]
    (if (zero? regval)
      [a b c d (inc line)]
      [a b c d (+ line linestojump)])))

(defn handle-add
  [a b c d instruction]
  (let
    [source (second (str/split instruction #" "))
     sourceval (case source "a" a "b" b "c" c "d" d)
     dest (nth (str/split instruction #" ") 2)
     [newa newb newc newd] (case dest
                             "a" [(+ a sourceval) b c d]
                             "b" [a (+ b sourceval) c d]
                             "c" [a b (+ c sourceval) d]
                             "d" [a b c (+ d sourceval)])]
    (case source
      "a" [0 newb newc newd]
      "b" [newa 0 newc newd]
      "c" [newa newb 0 newd]
      "d" [newa newb newc 0])))

(defn process-instructions
  [[a b c d instructions line done]]
  (let
    [instruction (nth instructions line "end")
     [newa newb newc newd newline newdone] (case (subs instruction 0 3)
                        "cpy" (concat (handle-cpy a b c d instruction) [(inc line) false])
                        "inc" (concat (handle-inc a b c d instruction) [(inc line) false])
                        "dec" (concat (handle-dec a b c d instruction) [(inc line) false])
                        "jnz" (concat (handle-jnz a b c d instruction line) [false])
                        "add" (concat (handle-add a b c d instruction) [(inc line) false])
                        "end" (vector a b c d line true))]
      [newa newb newc newd instructions newline newdone]))

(defn -main
  [& args]
  (time (let
    [instructions (get-input (first args))
     processed-instructions (take-while #(= false (nth % 6)) (iterate process-instructions [0 0 1 0 instructions 0 false]))]
    (println (count processed-instructions) (take 4 (last processed-instructions))))))
