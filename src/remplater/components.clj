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
    [org.apache.pdfbox.pdmodel.interactive.annotation PDAnnotationLink]
    [org.apache.pdfbox.pdmodel.interactive.action PDActionGoTo]
    [org.apache.pdfbox.pdmodel.interactive.documentnavigation.destination PDDestination PDPageFitWidthDestination]
    [java.awt Color]))

(defn merge-fig-opts [component & fig-opts]
  (update component 1 #(apply merge (concat fig-opts [%]))))

(defn document [fig-opts & children]
  children)

(defn page [{:as fig-opts :keys [document page]} & children]
  children)

(defn rect [{:keys [fill? stroke? fill-color line-width
                    cs x1 y1 x2 y2]
             :or {fill? true
                  stroke? false
                  line-width 1.0}
             :as fig-opts}
            & children]
  (let [;; TODO: add fig-opts->pdrect fn
        width (abs (- x2 x1))
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

(defn text [{:keys [cs x1 y1 x2 y2 text font font-size]
             :or {font-size 12}}
            & children]
  (let [font (or font
               (PDType1Font. Standard14Fonts$FontName/HELVETICA))]
    (.beginText cs)
    (.setFont cs font font-size)
    (.newLineAtOffset cs x1 (- y2 font-size))
    (.showText cs text)
    (.endText cs))
  children)

(defn margin [fig-opts & children]
  (let [mleft (or (:margin-left fig-opts) (:margin fig-opts) 0)
        mtop (or (:margin-top fig-opts) (:margin fig-opts) 0)
        mright (or (:margin-right fig-opts) (:margin fig-opts) 0)
        mbottom (or (:margin-bottom fig-opts) (:margin fig-opts) 0)
        new-fig-opts (fo/add-margin fig-opts mleft mtop mright mbottom)]
    (->> children
      (mapv #(merge-fig-opts % new-fig-opts)))))

(defn split [fig-opts & children]
  (let [split-points (fo/split fig-opts
                       (:direction fig-opts)
                       (:splits fig-opts))]
    (->> children
      (map-indexed
        (fn [index child]
          (merge-fig-opts child (get split-points index))))
      (vec))))

(defn grid [fig-opts & children]
  (let [cells-fig-opts (fo/grid fig-opts)]
    (->> cells-fig-opts
      (mapcat (fn [cell-fig-opts]
                (->> children
                  (mapv #(merge-fig-opts % fig-opts cell-fig-opts))))))))

;; TODO: add link-type to change PDPageFitWidthDestination
(defn page-link [{:keys [page cs]}
                 {:keys [target-page x1 y1 x2 y2] :as fig-opts}]
  (let [annotations (.getAnnotations page)
        annotation-link (PDAnnotationLink.)
        width (abs (- x2 x1))
        height (abs (- y2 y1))
        rect (PDRectangle. x1 y1 width height)
        go-to-action (PDActionGoTo.)
        destination (PDPageFitWidthDestination.)]
    (.setRectangle annotation-link rect)
    (.setPage destination target-page)
    (.setDestination go-to-action destination)
    (.setAction annotation-link go-to-action)
    (.add annotations annotation-link)))

(comment)
