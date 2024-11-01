(ns remplater.pdf
  (:import
    [org.apache.pdfbox.pdmodel PDDocument PDPage PDResources PDPageContentStream PDPageContentStream$AppendMode PDPatternContentStream]
    [org.apache.pdfbox.pdmodel.graphics.pattern PDTilingPattern]
    [org.apache.pdfbox.pdmodel.graphics.color PDPattern PDColor PDDeviceRGB]
    [org.apache.pdfbox.pdmodel.common PDRectangle]
    [java.awt Color]))

(def remarkable-2-page-size
  (PDRectangle. 1404 1872))

(defn make-document []
  (PDDocument.))

(defn save-document [^PDDocument document output-path]
  (.save document output-path))

(defn get-document-page [^PDDocument document page-index]
  (.getPage document page-index))

(defn add-page-to-document [document page]
  (.addPage document page))

(defn make-page [& [document]]
  (let [page (PDPage. remarkable-2-page-size)]
    (.setResources page (PDResources.))
    (when document
      (add-page-to-document document page))
    page))

(defn with-document [output-path f]
  (with-open [document (make-document)]
    (f document)
    (save-document document output-path)))

(defn with-page [document f]
  (let [page (make-page document)]
    (f page)))

(defn with-page-content-stream [document page f]
  (with-open [pcs (PDPageContentStream. document page PDPageContentStream$AppendMode/APPEND false)]
    (f pcs)))

;; TODO: deprecated?
(defn in-single-page-content [output-path f]
  (let [document (make-document)
        page (make-page)]
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
    (rem (int x1) 255)
    (rem (int y1) 255)
    150))

(defn with-graphics-state [cs f]
  (.saveGraphicsState cs)
  (f cs)
  (.restoreGraphicsState cs))
