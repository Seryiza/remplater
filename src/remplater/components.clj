(ns remplater.components
  (:require
    [clojure.math :as math]
    [remplater.fig-operations :as fo]
    [remplater.pdf :as pdf])

  (:import
    [org.apache.pdfbox.pdmodel PDDocument PDPage PDResources PDPageContentStream PDPageContentStream$AppendMode PDPatternContentStream]
    [org.apache.pdfbox.pdmodel.graphics.pattern PDTilingPattern]
    [org.apache.pdfbox.pdmodel.graphics.color PDPattern PDColor PDDeviceRGB]
    [org.apache.pdfbox.pdmodel.common PDRectangle]
    [org.apache.pdfbox.pdmodel.font PDFont PDType1Font Standard14Fonts$FontName]
    [java.awt Color]))

(defn rect [cs {:keys [fill? stroke? fill-color line-width
                       x1 y1 x2 y2]
                :or {fill? true
                     stroke? false
                     line-width 1.0}
                :as fig-opts}]
  (let [width (abs (- x2 x1))
        height (abs (- y2 y1))
        fill-color (or fill-color
                     (pdf/make-color-by-fig-position fig-opts))]
    (pdf/with-graphics-state cs
      (fn [cs]
        (.setNonStrokingColor cs fill-color)
        (.setLineWidth cs line-width)
        (.addRect cs x1 y1 width height)

        (cond
          (and fill? stroke?)
          (.fillAndStroke cs)

          fill?
          (.fill cs)

          stroke?
          (.stroke cs))

        (.closePath cs)))))

(defn text [cs {:keys [x1 y1 x2 y2 text font font-size]
                :or {font-size 12}}]
  (let [font (or font
               (PDType1Font. Standard14Fonts$FontName/HELVETICA))]
    (.beginText cs)
    (.setFont cs font font-size)
    (.newLineAtOffset cs x1 (- y2 font-size))
    (.showText cs text)
    (.endText cs)))

(comment)
