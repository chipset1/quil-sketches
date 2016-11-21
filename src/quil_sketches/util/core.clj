(ns quil-sketches.util.core)

(defn point-coll [point e]
  (and (> (:x point)
          (:x e))
       (> (:y point)
          (:y e))
       (< (:x point)
          (+ (:x e)
             (:w e)))
       (< (:y point)
          (+ (:y e)
             (:h e)))))

(defn chance?
  [p _]
  (< (rand 1)
     p))

(defn if-r [p e & [f]]
  (if (< (rand 1)
         p)
    e
    f))

;added this so
(defn random
  ([max] (random 0 max))
  ([min max]
   (if (>= min max)
     min
     (+ min (rand (- max min))))))

(defn random-int
  [min max]
  (int (random min max)))

(defn random-seq
  [min max]
  (repeatedly #(random min max)))

(defn grid
  "Returns list of positions (indices, coordinates) of a 2D array
  e.g (grid 3 3) => ([0 0] [0 1] [0 2]
                     [1 0] [1 1] [1 2]
                     [2 0] [2 1] [2 2])"
  ([w h]
   (grid 0 0 w h 1 1))
  ([w h sx sy]
   (grid 0 0 w h sx sy))
  ([start-x start-y w h step-x step-y]
   (for [x (range start-x w step-x)
         y (range start-y h step-y)]
     [x y])))

(defn random-grid
  [r-args1 r-args2 grid]
  (map (fn [[x y]]
         [(+ x (apply random r-args1))
          (+ y (apply random r-args2))])
       grid))
