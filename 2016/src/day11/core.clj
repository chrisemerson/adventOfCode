(ns day11.core
  (:gen-class))

(def initial-state {
  :1 '("strontium generator" "strontium chip" "plutonium generator" "plutonium chip")
  :2 '("thulium generator" "ruthenium generator" "ruthenium chip" "curium generator" "curium chip")
  :3 '("thulium chip")
  :4 '()
  :elevator :1
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
  (and
    (no-exposed-microchips? state)))

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

(defn get-min-moves-to-finish
  [state limit]
  (if (> limit 99)
    9999
    (if (finished? state)
      0
      (inc (apply min (map #(get-min-moves-to-finish % (inc limit)) (get-possible-next-positions state)))))))

(defn -main
  [& args]
  (println (get-min-moves-to-finish initial-state 0)))
