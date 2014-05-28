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

(defn block-value2 [values band-size]
  ;(reduce + (- 1 (/ (reduce + values) (count values) 255)))
  ;(println (type values) (type (first values)))
  
  (let [pixel-values (apply concat (for [row values] (for [i (range 0 (count row) band-size)] (pixel-value (subvec row i (+ i band-size))))))]
    (- 1 (/ (reduce + pixel-values) (count pixel-values) 255))
  )
  ;0.5
)

(defn svg-circle [{x :x
                   y :y
                   value :value
                   block-size :block-size}]
  (let [
    radius (float (* value (/ block-size 2)))
  ]
    (str "<circle cx=\"" (float x) "\" cy=\"" (float y) "\" r=\"" radius "\" />")
  )
)

(defn build-svg [svg-fn block-values]
  ;(time( println "Count: " (count block-values)))
  (println "generating string")
  (println (type block-values) (type (first block-values)))
  ;(time (vec block-values))
  (str
    "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n<svg baseProfile=\"tiny\" version=\"1.2\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:ev=\"http://www.w3.org/2001/xml-events\" xmlns:xlink=\"http://www.w3.org/1999/xlink\"><defs />"
    (time  (apply str (map svg-fn (filter (fn [a] (> (:value a))) block-values))))
    ;(apply str (pmap svg-fn (filter (fn [a] (> (:value a))) block-values)))
    ;(apply str (for [block block-values :when (> (:value block) 0.1)] (svg-fn block)))
    "</svg>"
  )
)

(defn save-svg [data-string]
  (println "saving")
  (time (spit "/home/mprintz/test.svg" data-string))
)

(defn get-block [pixels block-size width height band-size offset]
   (let [
    limit (count pixels)
    
    y (* (int (/ (mod (int (/ offset width)) width) block-size)) block-size)
    x (mod (int (/ offset block-size)) width)
    x_offset x
    y_limit  (min (+ y block-size) height)
    block-values (into [] ( for [y_i (range y y_limit)]
              (let [
                  y_offset (* y_i width)
                  x_limit (+ y_offset width)
                  start (* (+ x_offset y_offset           ) band-size)
                  end   (* (min (+ x_offset y_offset block-size) x_limit) band-size)
               ]
                (subvec pixels start end)
              )
            ))
    value (block-value2 block-values band-size)
    ;block-pixels (partition band-size (apply concat block-values))
    ;value (block-value (map pixel-value block-pixels))
  ]
    {
      :x (+ x (/ block-size 2))
      :y (+ y (/ block-size 2))
      :value value
    ;  ;:pixels block-pixels
      ;:value value
      :block-size block-size
    }
  )
)

(defn get-blocks [pixels block-size width height band-size]
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
    (println "getting blocks")
    (time (map (fn [offset] (get-block pixels block-size width height band-size offset)) offsets))
  )
)

(defn process-image [filename]
  (let [
        block-size 3
        imagefile (clojure.java.io/as-file filename)
        image-raster (. ImageIO read imagefile)
        raster (.getData image-raster)
        width (.getWidth image-raster)
        height (.getHeight image-raster)
        band-size (.getNumBands raster)
        pixel-data-array (into (vector-of :int) (.getPixels raster 0 0 width height #^ints (identity nil)))
        blocks (get-blocks pixel-data-array block-size width height band-size)
        svg-data (build-svg svg-circle blocks)
      ]
      ;(println (first pixels) (type pixels) (type (first pix)
      ;(println
        ;(first blocks)
        ;"\n"
        ;(subvec (count (first blocks)) pixel-data-array)
      ;)
      ;(println
      ;  (second blocks)
      ;)
      ;(vec blocks)
      (save-svg svg-data)
    )
  )

(defn -main [& args]
  (println "starting")
  (time
    (do 
      (process-image (first args))
      (println "Finishing:")
    )
  )
)
