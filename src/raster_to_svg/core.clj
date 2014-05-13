(ns raster-to-svg.core
  (:import [javax.imageio ImageIO]
           [java.awt.image BufferedImage])
  (:require [clojure.java.io])
  (:require [clojure.math.numeric-tower :as math])
)

(def nil-pixel '(255, 255, 255, nil))

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

(defn get-block [pixels block-size width height offset]
  (let [
    y (* (int (/ (mod (int (/ offset width)) width) block-size)) block-size)
    x (mod (int (/ offset block-size)) width)
    out-of-bound  (fn [pixel-offset y-offset] false)
    block-pixels-1 (time (vec (apply concat (for [y_i (range block-size) ] (let
        [ range_offset (+ offset (* y_i width)) ]
        (drop range_offset (take (+ range_offset block-size) pixels))
      ))) ))
    ;block-pixels-2 (time (vec (for [y_i (range block-size) x_i (range block-size)] (let
    ;    [ pixel_offset (+(+ offset x_i) (* y_i width)) ]
    ;    (nth pixels pixel_offset)
    ;    )) ))
    value (block-value (map pixel-value block-pixels-1))
  ]
    (println "a: " offset " : " block-pixels-1)
    ;(println "b: " offset " : " block-pixels-2)
    {
      :x (+ x (/ block-size 2))
      :y (+ y (/ block-size 2))
      ;:pixels block-pixels
      :value value
      :block-size block-size
    }
  )
)

(defn get-blocks [pixels block-size width height]
    ;    {
    ;      :x (* x block-size)
    ;      :y y
    ;      ;:pixels pixels
    ;      :value value
    ;      :block-size block-size
    ;    }
  (let [
    offsets (range 0 (* width height) (* block-size block-size))
    ;partitioned-offsets (partition 6 offsets)
  ]
    ;(apply concat (pmap (fn [s-offsets] (map (fn [s-offset] (get-block pixels block-size width height s-offset)) s-offsets)) partitioned-offsets))
    (map (fn [offset] (get-block pixels block-size width height offset)) offsets)
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
      ;(println
        ;(first blocks)
      ;)
      ;(println
      ;  (second blocks)
      ;)
      ;(vec blocks)
      (save-svg svg-data)
    )
  )

(defn -main [& args]
  (time (process-image (first args)))
)
