(ns raster-to-svg.core
  (:import [javax.imageio ImageIO]
           [java.awt.image BufferedImage])
  (:require [clojure.java.io])
  (:require [clojure.math.numeric-tower :as math])
)

(defn pixel-value [[r g b a]] ; Destructuring
  (* (/ (+ r g b) 3) (if (nil? a) 1 (/ a 255.0)))
)

(defn block-value [values]
  (- 1 (/ (reduce + values) (count values) 255))
)

(defn svg-circle [{x :x
                   y :y
                   value :value
                   block-size :block-size}]
  (let [
    radius (* value (/ block-size 2))
  ]
    (str "<circle cx=\"" x "\" cy=\"" y "\" r=\"" radius "\" />")
  )
)

(defn build-svg [svg-fn block-values]
  (time( println "Count: " (count block-values)))
  (str
    "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n<svg baseProfile=\"tiny\" version=\"1.2\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:ev=\"http://www.w3.org/2001/xml-events\" xmlns:xlink=\"http://www.w3.org/1999/xlink\"><defs />"
    (time (apply str (map svg-fn (filter (fn [a] (> (:value a))) block-values))))
    ;(apply str (pmap svg-fn (filter (fn [a] (> (:value a))) block-values)))
    ;(apply str (for [block block-values :when (> (:value block) 0.1)] (svg-fn block)))
    "</svg>"
  )
)

(defn save-svg [data-string]
  (println "saving")
  (time (spit "/home/mprintz/test.svg" data-string))
)

(defn get-blocks [pixels block-size width height]
  (let [
    empty-block (take block-size (repeat `(255 255 255 nil)))
    rows (partition width width (take width (repeat`(255 255 255 nil))) pixels)
    ;; Got to be a better way to create a list of num elements, but the for fuction works
    row-sets (for [row rows] (partition block-size block-size empty-block row))
    blocks (for [x (range (count (first row-sets))) y (range 0 (count row-sets) block-size) ]
      (let [
            pixels (apply concat (for [row (range y (+ y block-size))] (nth (nth row-sets y) x)))
            value (block-value (map pixel-value pixels))
        ]
        {
          :x (* x block-size)
          :y y
          ;:pixels pixels
          :value value
          :block-size block-size
        }
      )
    )
  ]
  (println "Vectorizing")
  (time (vec blocks))
  )
)

(defn process-image [filename]
  (let [
        block-size 8
        imagefile (clojure.java.io/as-file filename)
        image-raster (. ImageIO read imagefile)
        raster (.getData image-raster)
        width (.getWidth image-raster)
        height (.getHeight image-raster)
        pixel-data-array (.getPixels raster 0 0 width height #^ints (identity nil))
        pixels (partition (.getNumBands raster) pixel-data-array)
        blocks (get-blocks pixels block-size width height)
        svg-data (build-svg svg-circle blocks)
      ]
      (save-svg svg-data)
    )
  )

(defn -main [& args]
  (process-image (first args))
)
