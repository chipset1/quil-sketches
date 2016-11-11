(ns quil-sketches.caves
  (:require [quil-sketches.util.core :as u]))

(def cell-map [[1 0 0]
               [0 0 1]
               [1 0 0]])

(def cell-start-alive 0.45)

(defn neighbours
  "Determines all the neighbours of a given coordinate"
  [[x y]]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        :when (not= 0 dx dy)]
    [(+ dx x) (+ dy y)]))

(defn count-neighbours
  [grid coord]
  (->> (neighbours coord)
       (map #(get-in grid %1))
       (reduce +)))


(take 10 (repeatedly #(< (rand) cell-start-alive)))

(take 10 (repeatedly #(if (< (rand) cell-start-alive)
                        1
                        0)))
(defn cell
  "return a new randomly set cell"
  [chance-to-start-alive]
  (if (< (rand) cell-start-alive)
    1
    0))

(for [cell-x (take 10 (repeatedly #(cell 0.45)))
      cell-y (take 10 (repeatedly #(cell 0.45)))]
  [cell-x cell-y])

(defn cell-map
  [width height chance-to-start-alive]
  (for [cell-x (take width (repeatedly #(cell chance-to-start-alive)))
        cell-y (take height (repeatedly #(cell chance-to-start-alive)))]
    [cell-x cell-y]))

(cell-map 100 100 0.45)

(neighbours [1 1])

(->> (neighbours [0 1])
     (map #(get-in [[1 0 0]
                    [0 0 1]
                    [1 0 0]]
                   %)))


(count-neighbours cell-map [0 1])

(->> (neighbours [0 0])
     (map #(get-in [[1 0 0]
                    [0 0 1]
                    [1 0 0]]
                    %)))

(->> (neighbours [2 2])
     (map #(get-in [[1 0 0]
                    [0 0 1]
                    [1 0 0]]
                   %)))

(->> (neighbours [2 2])
     (map #(get-in [[1 0 0]
                    [0 0 1]
                    [1 0 0]]
                   %))
     (remove nil?))

(->> (neighbours [2 2])
     (keep #(get-in [[1 0 0]
                     [0 0 1]
                     [1 0 0]]
                    %)))

(defn count-neighbours
  [grid coord]
  (->> (neighbours coord)
       (keep #(get-in grid %1))
       (reduce +)))

(count-neighbours cell-map [0 1])

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



(apply-rule [[1 0 0]
             [0 0 1]
             [1 0 0]]
            [1 1]
            1
            2)

(let [map1 [[1 1 0]
            [0 0 1]
            [1 0 0]]
      death-limit 3
      birth-limit 2]
  (loop [new-map map1
         coords (u/grid 3 3 1 1)]
    (if (empty? coords)
      new-map
      (recur (assoc-in new-map
                       (first coords)
                       (apply-rule map1
                                   (first coords)
                                   death-limit
                                   birth-limit))
             (rest coords)))))
