(ns quil-sketches.caves
  (:require [quil.core :as q]
            [quil-sketches.util.core :as u]
            [quil.middleware :as m]))

(defn cell
  "return a new randomly set cell"
  [chance-to-start-alive]
  (if (< (rand 1) chance-to-start-alive)
    1
    0))

(defn cell-map
  [width height chance-to-start-alive]
  (vec (repeatedly height
                   (fn [] (vec (repeatedly width
                                           #(cell chance-to-start-alive)))))))

(defn neighbours
  "Determines all the neighbours of a given coordinate"
  [[x y]]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        :when (not= 0 dx dy)]
    [(+ dx x) (+ dy y)]))

(defn on-edge?
  [grid coord]
  (let [row (first coord)
        col (second coord)
        width (dec (count (first grid)))
        height (dec (count grid))]
    (or (= row 0)
        (= row height)
        (= col 0)
        (= col width))))

(defn count-neighbours
  [grid coord]
  (->> (neighbours coord)
       (keep #(if (on-edge? grid coord)
                1
                (get-in grid %1)))
       (reduce +)))

(defn apply-rule
  [cell-map coord death-limit birth-limit]
  (let [cell (get-in cell-map coord)
        nbs (count-neighbours cell-map coord)]
    (if (= cell 1)
        (if (< nbs death-limit)
          0
          1)
        (if (> nbs birth-limit)
          1
          0))))

(defn sim-step
  [cell-map width height death-limit birth-limit]
  (loop [new-cell-map cell-map
         coords (u/grid width
                        height
                        1
                        1)]
    (if (empty? coords)
      new-cell-map
      (recur (assoc-in new-cell-map
                       (first coords)
                       (apply-rule cell-map
                                   (first coords)
                                   death-limit
                                   birth-limit))
             (rest coords)))))

(defn generate-map
  [initial-map steps chance-to-start-alive death-limit birth-limit]
  (let [width (count (first initial-map))
        height (count initial-map)]
    (loop [s steps
           m (sim-step initial-map
                       width
                       height
                       death-limit
                       birth-limit)]
      (if (= s 1)
        m
        (recur (dec s)
               (sim-step m width height death-limit birth-limit))))))

(def board (generate-map (cell-map 50 50 0.45)
                         10
                         0.4
                         3
                         4))

(defn draw []
  (let [cell-width 10
        cell-height 10]
    (q/background (q/unhex "3355AA"))
    (doall (map-indexed (fn [i row]
                          (doall (map-indexed (fn [j cell]
                                                (when (= cell 1)
                                                  (q/no-stroke)
                                                  (q/fill 68,51,51)
                                                  (q/rect (* i 10)
                                                          (* j 10)
                                                          10
                                                          10)))
                                              row)))
                        board))))

(q/defsketch quil-sketches
  :size [500 500]
  :draw draw
  :features [:keep-on-top]
  :middleware [m/pause-on-error])
