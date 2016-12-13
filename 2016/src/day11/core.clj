(ns day11.core
  (:gen-class))

;(def initial-state {
;  :1 '("strontium generator" "strontium chip" "plutonium generator" "plutonium chip")
;  :2 '("thulium generator" "ruthenium generator" "ruthenium chip" "curium generator" "curium chip")
;  :3 '("thulium chip")
;  :4 '()
;  :elevator :1
;})

(def initial-state {
  :1 '()
  :2 '()
  :3 '("hydrogen generator" "lithium generator" "lithium chip" "hydrogen chip")
  :4 '()
  :elevator :3
})

(defn get-possible-items-to-take
  [state]
  (concat
    (map list ((:elevator state) state))
    (filter #(not (= (first %) (second %)))
      (for
        [a ((:elevator state) state)
         b ((:elevator state) state)]
        (list a b)))))

(defn get-possible-floors
  [state]
  (case (:elevator state)
    :1 '(:2)
    :2 '(:1 :3)
    :3 '(:2 :4)
    :4 '(:3)))

(defn finished?
  [state]
  (and
    (empty? (:1 state))
    (empty? (:2 state))
    (empty? (:3 state))))

(defn no-exposed-microchips-on-floor?
  [itemsonfloor]
  (let
    [chips (set (map #(nth (re-matches #"^(.*) chip$" %) 1) (filter #(re-find #" chip$" %) itemsonfloor)))
     generators (set (map #(nth (re-matches #"^(.*) generator$" %) 1) (filter #(re-find #" generator$" %) itemsonfloor)))]
     (if (empty? generators)
       true
       (if (empty? (remove generators chips))
         true
         false))))

(defn no-exposed-microchips?
  [state]
  (and
    (no-exposed-microchips-on-floor? (:1 state))
    (no-exposed-microchips-on-floor? (:2 state))
    (no-exposed-microchips-on-floor? (:3 state))
    (no-exposed-microchips-on-floor? (:4 state))))

(defn valid-state?
  [state]
  (no-exposed-microchips? state))

(defn make-move
  [state [floor items]]
  (let
    [currentfloor (:elevator state)
     currentflooritems (currentfloor state)
     nextflooritems (floor state)]
    (assoc state currentfloor (remove (set items) currentflooritems) floor (concat nextflooritems items) :elevator floor)))

(defn get-possible-next-positions
  [state]
  (filter valid-state?
    (map #(make-move state (apply vector %))
      (for
        [items (get-possible-items-to-take state)
         floor (get-possible-floors state)]
        (list floor items)))))

(defn score
  [state]
  (+
    (count (:3 state))
    (* (count (:2 state)) 2)
    (* (count (:1 state)) 3)))

(defn sort-by-score
  [x]
  (sort #(compare (score %1) (score %2)) x))

(defn next-positions-by-score
  [state]
  (sort-by-score (get-possible-next-positions state)))

(defn get-min-moves-to-finish
  [state depth limit visitedstates]
  (if (finished? state)
    [depth (assoc visitedstates state depth)]
    (if (contains? visitedstates state)
      [(get visitedstates state) visitedstates]
      (let
        [reducefunc (fn [[returneddepth returnedvisitedstates] nextstate] (get-min-moves-to-finish nextstate (inc depth) returneddepth returnedvisitedstates))]
        (if (> depth limit)
          [limit (assoc visitedstates state limit)]
          (if (= 1 (count (next-positions-by-score state)))
            (get-min-moves-to-finish (first (next-positions-by-score state)) (inc depth) limit visitedstates)
            (reduce reducefunc [limit visitedstates] (next-positions-by-score state))))))))

(defn -main
  [& args]
  (println (first (get-min-moves-to-finish initial-state 0 (Integer. (first args)) {}))))
