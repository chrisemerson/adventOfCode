(ns day13.core
  (:gen-class))

(def goal [31 39])

(defn wall?
  [x y n]
  (let
    [number (+ (* x x) (* 3 x) (* 2 x y) y (* y y) n)
     binnum (Integer/toString number 2)]
    (odd? (count (filter #(= \1 %) binnum)))))

(defn get-next-positions
  [x y oldx oldy n]
  (filter
    #(not (or (wall? (first %) (second %) n) (and (= oldx (first %)) (= oldy (second %)))))
    (list [(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)])))

(defn distance-from-goal
  [x y]
  (+ (* (- (first goal) x)(- (first goal) x)) (* (- (second goal) y) (- (second goal) y))))

(defn sort-by-distance
  [x]
  (sort #(compare (distance-from-goal (first %1) (second %1)) (distance-from-goal (first %2) (second %2))) x))

(defn get-min-moves-to-goal
  [x y oldx oldy n]
  (if (and (= x 31) (= y 39))
    0
    (let
      [nextpositions (sort-by-distance (get-next-positions x y oldx oldy n))]
      (if (= (count nextpositions) 0)
        9999
        (apply min (map #(get-min-moves-to-goal (first %) (second %) x y n) nextpositions))))))

(defn -main
  [& args]
  (println (get-min-moves-to-goal 1 1 0 0 (Integer. (first args)))))
