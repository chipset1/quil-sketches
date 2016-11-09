(ns quil-sketches.glitch-art2
  (:require [quil.core :as q]
            [quil-sketches.util.core :as u]
            [quil-sketches.vector :as v]
            [quil.middleware :as m]))

;image dimensions 1500, 1131
(def width 1500)
(def height 1000)

(def images ["http://i.imgur.com/vgL3odh.jpg"
             "http://i.imgur.com/5Ur9oba.png"
             "http://i.imgur.com/LS4SV8D.jpg"
             "http://i.imgur.com/6VPzGn1.jpg"])

(defn setup []
   ;images/vD07h30.jpg
  {:image (q/load-image "http://i.imgur.com/LS4SV8D.jpg")
   :brush {:pos [0 0]
           :quad {:size [10 10]}
           :chance 0.2
           :random-uv [[-20 40] [-20 30]]
           :grid {:size [200 200]
                  :step [10 10]
                  :rand [[0 0] [0 0]]}}})

(defn random-uv
  [r-args1 r-args2 vd]
  (map (fn [v]
         (concat v [(+ (v/x v)
                       (apply u/random r-args1))
                    (+ (v/y v)
                       (apply u/random r-args2))]))
       vd))

(defn quad-data
  "return data for quads to be applied to the q/vertex fn"
  [x y w h]
  [[x y]
   [x (+ y h)]
   [(+ x w) (+ y h)]
   [(+ x w) y]])

(defn mouse-moved
  [old-state event]
  (assoc-in old-state
            [:brush :pos]
            (vector (:x event)
                    (:y event))))

(defn apply-vertexs
  [image v-args]
  (q/begin-shape :quads)
  (q/no-stroke)
  (q/texture image)
  (doseq [args v-args]
    (apply q/vertex args))
  (q/end-shape))

(defn brush-data
  "(brush-data {:brush {:pos [0 0]
                     :quad {:size [10 10]}
                     :random-uv [[40] [10]]
                     :grid {:size [150 150]
                            :step [10 10]
                            :rand [[-30 30] [-20 50]]}}})"
  [state]
  (let [brush (:brush state)
        grid (:grid brush)
        grid-params (concat (:pos brush)
                            (v/add (:pos brush)
                                   (:size grid))
                            (:step grid))]

    (->> (apply u/grid grid-params)
         (u/random-grid (first (:rand grid))
                        (second (:rand grid)))
         (map (fn [[x y]]
                  (quad-data x
                             y
                             (-> brush
                                 :quad
                                 :size
                                 first)
                             (-> brush
                                 :quad
                                 :size
                                 second))))

         (map (partial random-uv
                       (-> state :brush :random-uv first)
                       (-> state :brush :random-uv second))))))

(defn draw
  [state]
  (let []
    (if (q/mouse-pressed?)
      (doall (->> (brush-data state)
                  (remove (partial u/chance? (:chance (:brush state))))
                  (map (partial apply-vertexs (:image state)))))
      (q/rect (v/x (:pos (:brush state)))
              (v/y (:pos (:brush state)))
              10
              10))))

(q/defsketch quil-sketches
  :size [width height]
  :setup setup
  :draw draw
  :mouse-dragged mouse-moved
  :mouse-moved mouse-moved
  :renderer :p3d
  :features [:keep-on-top]
  :middleware [m/pause-on-error
               m/fun-mode])
