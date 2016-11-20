(ns quil-sketches.caves
  (:require [quil.core :as q]
            [quil-sketches.util.core :as u]
            [quil.middleware :as m]))

(declare cell-map)

(def sketch-width 300)
(def sketch-height 300)
(def cell-size 5)
(def board (cell-map (/ sketch-width cell-size)
                     (/ sketch-height cell-size)
                     0.46))

(defn rand-cell
  "return an alive or dead cell based on "
  [chance-to-start-alive]
  (if (< (rand 1) chance-to-start-alive)
    1
    0))

(defn cell-map
  [width height chance-to-start-alive]
  (vec (repeatedly height
                   (fn [] (vec (repeatedly width

                                           #(rand-cell chance-to-start-alive)))))))

(defn neighbours
  [[x y]]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        :when (not= 0 dx dy)]
    [(+ dx x) (+ dy y)]))

(defn on-edge?
  [cell-map [row col]]
  (let [width (dec (count (first cell-map)))
        height (dec (count cell-map))]
    (or (= row 0)
        (= row height)
        (= col 0)
        (= col width))))

(defn alive-neighbours
  [cell-map coord]
  (->> (neighbours coord)
       (keep #(get-in cell-map %))
       (reduce +)))

(defn survival-rule
  "if a cell is alive: empty it if it has less filled neighbours than the death-limt
   cell is empty: fill it if its neighbours is less than the birthlimit
  cells on edges have fixed neighbours to create a cave boundary "
  [birth-limit death-limit]
  (fn [cell-map coord]
    (let [cell (get-in cell-map coord)
          nbs (if (on-edge? cell-map coord)
                5
                (alive-neighbours cell-map coord))]
      (if (= cell 1)
        (if (< nbs death-limit)
          0
          1)
        (if (> nbs birth-limit)
          1
          0)))))

(defn treasure-rule
  [limit]
  (fn [cell-map coord]
    (let [cell (get-in cell-map coord)
          nbs (alive-neighbours cell-map coord)]
      (if (and (= cell 0)
               (>= nbs limit))
        2
        cell))))

(defn simulation
  [rule-fn cell-map]
  (let [width (count cell-map)
        height (count (first cell-map))]
    (loop [new-cell-map cell-map
          coords (u/grid width
                         height
                         1
                         1)]
     (if (empty? coords)
       new-cell-map
       (recur (assoc-in new-cell-map
                        (first coords)
                        (rule-fn cell-map (first coords)))
              (rest coords))))))

(defn generate-map
  [rule-fn initial-map steps]
  (loop [s steps
         m (simulation rule-fn
                       initial-map)]
    (if (= s 1)
      m
      (recur (dec s) (simulation rule-fn m)))))

(defn draw []
  (q/background (q/unhex "3355AA"))
  (q/no-stroke)
  (doall (map-indexed (fn [i row]
                        (doall (map-indexed (fn [j cell]
                                              (when (= cell 2)
                                                (q/fill 0)
                                                (q/rect (* j 10)
                                                           (* i 10)
                                                           10
                                                           10))
                                              (when (= cell 1)
                                                (q/fill 68 51 51)
                                                (q/rect (* j 10)
                                                        (* i 10)
                                                        10
                                                        10)))
                                            row)))
                      (sim-with-treasure (simulation c1
                                                     10 10
                                                     3 4)
                                         10
                                         10
                                         6))))

(q/defsketch quil-sketches
  :size [sketch-width sketch-height]
  :draw draw
  :features [:keep-on-top]
  :middleware [m/pause-on-error])
