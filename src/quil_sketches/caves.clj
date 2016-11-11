(ns quil-sketches.caves)

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

(->> (neighbours [1 1])
     (map #(get-in [[1 0 0]
                    [0 0 1]
                    [1 0 0]]
                   %)))


(count-neighbours cell-map [1 1])
