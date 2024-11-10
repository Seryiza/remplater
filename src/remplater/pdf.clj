(ns remplater.pdf
  (:import
    [org.apache.pdfbox.pdmodel PDDocument PDPage PDResources PDPageContentStream PDPageContentStream$AppendMode PDPatternContentStream]
    [org.apache.pdfbox.pdmodel.graphics.pattern PDTilingPattern]
    [org.apache.pdfbox.pdmodel.graphics.color PDPattern PDColor PDDeviceRGB]
    [org.apache.pdfbox.pdmodel.common PDRectangle]
    [java.awt Color]))

(def a4-page-size
  PDRectangle/A4)

(def remarkable-2-page-size
  (PDRectangle. 1404 1872))

(def remarkable-2-horizontal-page-size
  (PDRectangle. 1872 1404))

(defn make-document []
  (PDDocument.))

(defn save-document [^PDDocument document output-path]
  (.save document output-path))

(defn get-document-page [^PDDocument document page-index]
  (.getPage document page-index))

(defn add-page-to-document [document page]
  (.addPage document page))

(defn make-page [{:keys [size document]}]
  (let [page (PDPage. (or size remarkable-2-page-size))]
    (.setResources page (PDResources.))
    (when document
      (add-page-to-document document page))
    page))

(defn with-document [output-path f]
  (with-open [document (make-document)]
    (f document)
    (save-document document output-path)))

(defn with-page [document f]
  (let [page (make-page {:document document})]
    (f page)))

(defn with-page-content-stream [document page f]
  (with-open [pcs (PDPageContentStream. document page PDPageContentStream$AppendMode/APPEND true)]
    (f pcs)))

;; TODO: deprecated?
(defn in-single-page-content [output-path f]
  (let [document (make-document)
        page (make-page {})]
    (add-page-to-document document page)
    (with-page-content-stream document page f)
    (save-document document output-path)))

(defn page->pdrect [page]
  (.getBBox page))

(defn pdrect->fig-opts [^PDRectangle pdrect]
  {:x1 (.getLowerLeftX pdrect)
   :y1 (.getLowerLeftY pdrect)
   :x2 (.getUpperRightX pdrect)
   :y2 (.getUpperRightY pdrect)})

(defn make-color [r g b & [a]]
  (if a
    (Color. r g b a)
    (Color. r g b)))

(defn make-color-by-fig-position [{:keys [x1 y1] :as fig-opts}]
  (make-color
    (int (abs (rem x1 255)))
    (int (abs (rem y1 255)))
    (int (+ 120 (abs (rem (+ x1 y1) 100))))))

;; TODO: use it in render-tree fn?
(defn with-graphics-state [cs f]
  (.saveGraphicsState cs)
  (let [result (f cs)]
    (.restoreGraphicsState cs)
    result))

(defn draw-circle [cs x y r]
  (let [magic (* r 0.551784)]
    (doto cs
      (.moveTo x (+ y r))
      (.curveTo (+ x magic)
        (+ y r)
        (+ x r)
        (+ y magic)
        (+ x r)
        y)
      (.curveTo (+ x r)
        (- y magic)
        (+ x magic)
        (- y r)
        x
        (- y r))
      (.curveTo (- x magic)
        (- y r)
        (- x r)
        (- y magic)
        (- x r)
        y)
      (.curveTo (- x r)
        (+ y magic)
        (- x magic)
        (+ y r)
        x
        (+ y r))
      (.closePath))))
