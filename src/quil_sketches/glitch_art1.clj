(ns quil-sketches.glitch-art1
  (:require [quil.core :as q]
            [quil-sketches.util.core :as u]
            [quil-sketches.vector :as v]
            [quil.middleware :as m]))

;image dimensions 1500, 1131
(def width 500)
(def height 500)

(defn vertex-data
  [x y w h]
  [[x y]
   [x (+ y h)]
   [(+ x w) (+ y h)]
   [(+ x w) y]])

(defn apply-vertexs
  [image v-args]
  (q/begin-shape :quads)
  (q/no-stroke)
  (q/texture image)
  (doall (map (fn [args]
                (apply q/vertex args))
              v-args))
  (q/end-shape))

(defn random-uv
  [r-args1 r-args2 vd]
  (map (fn [v]
         (concat v [(+ (v/x v)
                       (apply q/random r-args1))
                    (+ (v/y v)
                       (apply q/random r-args2))]))
       vd) )

(defn glitch1 [image]
  (q/tint 255 200)
  (dotimes [i 3]
    (->> (u/grid width height 60 60)
         (u/random-grid [-30 30] [-30 30])
         (map (fn [[x y]]
                (vertex-data x
                             y
                             (q/random 30 80)
                             (q/random 30 80))))
         (map (partial random-uv [100] [100]))
         (map (partial apply-vertexs image))
         doall)))

(defn glitch2
  [image]
  (q/tint 255 120)
  (->> (u/grid width height 10 10)
       (u/random-grid [-50 50] [-50 50])
       (map (fn [[x y]]
              (vertex-data x
                           y
                           (q/random 10)
                           (q/random 10 50))))
       (map (partial random-uv [-300 100] [-200 100]))
       (remove (partial u/chance? 0.4))
       (map (partial apply-vertexs image))
       doall)
  (->> (u/grid width height 10 10)
       (u/random-grid [-50 50] [-50 50])
       (map (fn [[x y]]
              (vertex-data x
                           y
                           (q/random 10 50)
                           (q/random 10))))
       (map (partial random-uv [20 30] [20 30]))
       (remove (partial u/chance? 0.2))
       (map (partial apply-vertexs image))
       doall))

(defn draw []
  (let [image (q/load-image "images/vD07h30.jpg")]
    #_(glitch1 image)
    (glitch2 image)
    #_(q/save-frame "scratch-output/glitch-####.gif")
    ))

(q/defsketch quil-sketches
  :size [width height]
  :draw draw
  :renderer :p3d
  :features [:keep-on-top]
  :middleware [m/pause-on-error])
