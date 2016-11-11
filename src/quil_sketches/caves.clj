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

(take 10 (repeatedly #(cell 0.45)))

(repeatedly 10
            (fn [] (take 10 (repeatedly #(cell 0.45)))))



(defn cell-map
  [width height chance-to-start-alive]
  (vec (repeatedly height
                   (fn [] (->> #(cell chance-to-start-alive)
                               repeatedly
                               (take width)
                               vec)))))

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

(let [cell-map [[1 1 0]
                [0 0 1]
                [1 0 0]]
      death-limit 3
      birth-limit 2]
  (loop [new-cell-map cell-map
         coords (u/grid 3 3 1 1)]
    (if (empty? coords)
      new-cell-map
      (recur (assoc-in new-cell-map
                       (first coords)
                       (apply-rule cell-map
                                   (first coords)
                                   death-limit
                                   birth-limit))
             (rest coords)))))


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

(generate-map 2 3 3 0.45 2 2)

(cell-map 3 3 0.45)

(defn generate-map
  [steps width height chance-to-start-alive death-limit birth-limit]
  (loop [s steps
         m (sim-step (cell-map width
                               height
                               chance-to-start-alive)
                     width
                     height
                     death-limit
                     birth-limit)]
    (if (= s 0)
      m
      (recur (dec s)
             (sim-step m width height death-limit birth-limit)))))
