(ns raster-to-svg.core
  (:import [javax.imageio ImageIO]
           [java.awt.image BufferedImage])
  (:require [clojure.java.io])
  (:require [clojure.math.numeric-tower :as math])
)

(defn get-pixels-for-block [raster block-x block-y block-size]
  (for [x (range (* block-x block-size) (* (inc block-x) block-size))
        y (range (* block-y block-size) (* (inc block-y) block-size))]
    (try 
      (.getPixel raster x y nil)
      (catch Exception e `(255 255 255 255))
    )
    ))

(defn pixel-value [[r g b a]] ; Destructuring
    (* (/ (+ r g b) 3) (if (nil? a) 1 (/ a 255.0)))
 )

(defn block-value [values]
  (- 1 (/ (reduce + values) (count values) 255))
)

(defn process-block [raster block-x block-y block-size]
 (let [
  block-mid (/ block-size 2)
  pixels (get-pixels-for-block raster block-x, block-y, block-size)
  pixel-values (for [pixel pixels] (pixel-value pixel))
 ]
  ;(print block-x " " block-y "\n")
  {
    :x (+ (* block-x block-size) block-mid)
    :y (+ (* block-y block-size) block-mid)
    :value (block-value pixel-values)
    :block-size block-size
  }
 )
)

(defn svg-circle [{x :x
                   y :y
                   value :value
                   block-size :block-size}]
  (let [
    radius (* value (/ block-size 2))
  ]
    (format "<circle cx=\"%d\" cy=\"%d\" r=\"%f\" />" x y (float radius))
  )
)

(defn build-svg [svg-fn block-values]
  (str
    "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n<svg baseProfile=\"tiny\" version=\"1.2\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:ev=\"http://www.w3.org/2001/xml-events\" xmlns:xlink=\"http://www.w3.org/1999/xlink\"><defs />"
    (apply str (map svg-fn (filter (fn [a] (> (:value a))) block-values)))
    ;(apply str (pmap svg-fn (filter (fn [a] (> (:value a))) block-values)))
    ;(apply str (for [block block-values :when (> (:value block) 0.1)] (svg-fn block)))
    "</svg>"
  )
)

(defn save-svg [data-string]
  (spit "/home/mprintz/test.svg" data-string)
)

(defn process-image [filename]
  (let [
        block-size 8
        imagefile (clojure.java.io/as-file filename)
        raster (.getData (. ImageIO read imagefile))
        width-in-blocks (math/ceil (/ (.getWidth raster) block-size))
        height-in-blocks (math/ceil (/ (.getHeight raster) block-size))
        block-coords (for [x (range width-in-blocks) y (range height-in-blocks)] [x y])
        values (map (fn [[x y]] (process-block raster x y block-size)) block-coords)
        svg-data (build-svg svg-circle values)
      ]
      
      (save-svg svg-data)
    )
  )

(defn -main [& args]
  (process-image (first args))
)
