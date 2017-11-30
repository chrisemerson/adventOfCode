(ns day7.core
  (:gen-class)
  (:require [clojure.string :as str]))

(defn get-input
  [filename]
  (str/split-lines (str/trim (slurp filename))))

(defn follows-abba-pattern
  [ipv7]
  (re-find #"([A-Za-z])(?!\1)([A-Za-z])\2\1" ipv7))

(defn follows-abba-inside-square-brackets
  [ipv7]
  (re-find #"\[[^\]]*([A-Za-z])(?!\1)([A-Za-z])\2\1.*\]" ipv7))

(defn supports-tls
  [ipv7]
  (and
    (follows-abba-pattern ipv7)
    (not (follows-abba-inside-square-brackets ipv7))))

(defn supports-ssl
  [ipv7]
  (or
    (re-find #"(?:^[^\[]*|\][^\[]*)([A-Za-z])(?!\1)([A-Za-z])\1.*\[[^\]]*\2\1\2.*\]" ipv7)
    (re-find #"\[[^\]]*([A-Za-z])(?!\1)([A-Za-z])\1[^\]]*\]([^\[]|\[[^\]]*\])*\2\1\2" ipv7)))

(defn -main
  [& args]
  (let
    [ips (get-input (first args))]
    (println "TLS: " (count (filter supports-tls ips)))
    (println "SSL: " (count (filter supports-ssl ips)))))
