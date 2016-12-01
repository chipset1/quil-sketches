(ns quil-sketches.caves
  "Based on https://gamedevelopment.tutsplus.com/tutorials/generate-random-cave-levels-using-cellular-automata--gamedev-9664"
  (:require [quil.core :as q]
            [quil-sketches.util.core :as u]
            [quil.middleware :as m]))

(def sketch-width 500)
(def sketch-height 500)
(def cell-size 5)

(defn rand-cell
  "return dead cell (0) or an alive cell (1) based on a chance percentage from 0.0 - 1.0
   e.g (rand-cell 0.5)"
  [chance-to-start-alive]
  (if (< (rand 1) chance-to-start-alive)
    :alive
    :dead))

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
       (filter (partial = :alive))
       count))

(defn survival-rule
  "If a cell is alive: kill it if it has less alive neighbours than the death-limit.
   If a cell is dead: make it alive if its alive eighbours is greater than the birthlimit.
   Cells on edges have fixed neighbours to create a solid boundary around cave"
  [birth-limit death-limit]
  (fn [cell-map coord]
    (let [cell (get-in cell-map coord)
          nbs (if (on-edge? cell-map coord)
                5
                (alive-neighbours cell-map coord))]
      (if (= cell :alive)
        (if (< nbs death-limit)
          :dead
          :alive)
        (if (> nbs birth-limit)
          :alive
          :dead)))))

(defn treasure-rule
  [limit]
  (fn [cell-map coord]
    (let [cell (get-in cell-map coord)
          nbs (alive-neighbours cell-map coord)]
      (if (and (= cell :dead)
               (>= nbs limit))
        :treasure
        cell))))

(defn simulation
  [rule-fn cell-map]
  (let [width (count cell-map)
        height (count (first cell-map))]
    (loop [new-cell-map cell-map
           coords (u/grid width height)]
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


(defn setup []
  {:cell-map (cell-map (/ sketch-width cell-size)
                       (/ sketch-height cell-size)
                       0.46)})

(defn mouse-pressed
  [old-state event]
  {:cell-map (cell-map (/ sketch-width cell-size)
                       (/ sketch-height cell-size)
                       0.46)})

(defn draw [state]
  (q/background (q/unhex "3355AA"))
  (q/no-stroke)
  (doall (map-indexed (fn [i row]
                        (doall (map-indexed (fn [j cell]
                                              (when (= cell :treasure)
                                                (q/fill 241 212 55)
                                                (q/rect (* j cell-size)
                                                        (* i cell-size)
                                                        cell-size
                                                        cell-size))
                                              (when (= cell :alive)
                                                (q/fill 68 51 51)
                                                (q/rect (* j cell-size)
                                                        (* i cell-size)
                                                        cell-size
                                                        cell-size)))
                                            row)))
                      (->> (generate-map (survival-rule 4 4) ;; change these values to affect cave gen
                                         (:cell-map state)
                                         6)
                           (simulation (treasure-rule 5))))))


(q/defsketch quil-sketches
  :size [sketch-width sketch-height]
  :setup setup
  :mouse-pressed mouse-pressed
  :draw draw
  :features [:keep-on-top]
  :middleware [m/fun-mode
               m/pause-on-error])
