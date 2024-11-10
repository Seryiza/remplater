(ns remplater.components
  (:require
    [remplater.fig-operations :as fo]
    [remplater.pdf :as pdf]
    [remplater.render :as r])

  (:import
    [org.apache.pdfbox.pdmodel PDDocument PDPage PDResources PDPageContentStream PDPageContentStream$AppendMode PDPatternContentStream]
    [org.apache.pdfbox.pdmodel.graphics.pattern PDTilingPattern]
    [org.apache.pdfbox.pdmodel.graphics.color PDPattern PDColor PDDeviceRGB]
    [org.apache.pdfbox.pdmodel.common PDRectangle]
    [org.apache.pdfbox.pdmodel.font PDFont PDType1Font Standard14Fonts$FontName]
    [org.apache.pdfbox.pdmodel.interactive.annotation PDAnnotationLink PDBorderStyleDictionary]
    [org.apache.pdfbox.pdmodel.interactive.action PDActionGoTo]
    [org.apache.pdfbox.pdmodel.interactive.documentnavigation.destination PDDestination PDPageFitWidthDestination]
    [java.awt Color]))

;; TODO: extract it to render?
(defn merge-fig-opts [component & fig-opts]
  (let [component (cond
                    (fn? component) [component {}]
                    :else component)]
    (update component 1 #(apply merge (concat fig-opts [%])))))

(defmethod r/render :div [_ attrs & children]
  children)

(defmethod r/render :rect [_ {:keys [fill? stroke? fill-color line-width
                                     x1 y1 x2 y2]
                              :or {fill? true
                                   stroke? false
                                   line-width 1.0}
                              :as fig-opts}
                           & children]
  (let [;; TODO: add fig-opts->pdrect fn
        width (abs (- x2 x1))
        height (abs (- y2 y1))
        fill-color (cond
                     (fn? fill-color) (fill-color fig-opts)
                     (some? fill-color) fill-color
                     :else (pdf/make-color-by-fig-position fig-opts))]
    (pdf/with-graphics-state r/*cs*
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

        (.closePath cs)))
    children))

(defmethod r/render :circle [_ {:as fig-opts :keys [radius fill-color]} & children]
  (let [{:keys [x y]} (fo/rect->center fig-opts)]
    (pdf/with-graphics-state r/*cs*
      (fn [cs]
        (doto cs
          (.setNonStrokingColor fill-color)
          (pdf/draw-circle x y radius)
          (.fill)))))
  children)

(defmethod r/render :line [_ {:keys [x1 y1 x2 y2 width color cap-style]
                              :or {width 1
                                   cap-style 0
                                   color Color/BLACK}}
                           & children]
  (pdf/with-graphics-state r/*cs*
    (fn [cs]
      (doto cs
        (.setLineWidth width)
        (.setStrokingColor color)
        (.moveTo x1 y1)
        (.lineTo x2 y2)
        (.setLineCapStyle cap-style)
        (.stroke))))
  children)

(defmethod r/render :border [_ {:as fig-opts :keys [border-left border-top border-right border-bottom]}
                             & children]
  (let [make-border-line-fn
        (fn [border-type border-opts]
          [:line (merge fig-opts (fo/rect->border-line fig-opts border-type))])]
    (->> [(when border-left
            (make-border-line-fn :left border-left))
          (when border-right
            (make-border-line-fn :right border-right))]
      (filter some?)
      (vec))))

;; TODO: add text-align attrs
;; TODO: add align attr for children
(defmethod r/render :text [_ {:as fig-opts
                              :keys [x1 y1 x2 y2
                                     halign valign text font font-size fill-color
                                     text-offset children-offset]
                              :or {font-size 12
                                   fill-color Color/BLACK
                                   valign :top
                                   halign :left
                                   children-offset 0}}
                           & children]
  (let [font (or
               (get-in r/*document* [1 :fonts font])
               (get-in r/*document* [1 :fonts :default])
               (PDType1Font. Standard14Fonts$FontName/HELVETICA))]
    (let [block-width (abs (- x2 x1))
          block-height (abs (- y2 y1))
          text-height font-size
          text-width (* font-size (/ (.getStringWidth font text) 1000))
          text-offset (or text-offset (/ text-height 4))
          free-space-x (- block-width text-width)
          free-space-y (- block-height text-height)
          text-pos-x (case halign
                       :left x1
                       :center (+ x1 (/ free-space-x 2))
                       :right (+ x1 free-space-x))
          text-pos-y (case valign
                       :top (- y2 font-size)
                       :center (+ y1 (/ free-space-y 2))
                       :bottom y1)

          child-fig-opts {:x1 text-pos-x
                          :y1 (+ text-pos-y children-offset)
                          :x2 (+ text-pos-x text-width)
                          :y2 (+ text-pos-y text-height children-offset)}]
      (if (not-empty children)
        ;; TODO: make it as more elegant way?
        (->> (concat children [[:text fig-opts]])
          (mapv #(merge-fig-opts % child-fig-opts)))

        (pdf/with-graphics-state r/*cs*
          (fn [cs]
            (.beginText cs)
            (.setNonStrokingColor cs fill-color)
            (.setFont cs font font-size)
            (.newLineAtOffset cs text-pos-x (+ text-pos-y text-offset))
            (.showText cs text)
            (.endText cs)))))))

(defmethod r/render :margin [_ fig-opts & children]
  (let [mleft (or (:margin-left fig-opts) (:margin fig-opts) 0)
        mtop (or (:margin-top fig-opts) (:margin fig-opts) 0)
        mright (or (:margin-right fig-opts) (:margin fig-opts) 0)
        mbottom (or (:margin-bottom fig-opts) (:margin fig-opts) 0)
        new-fig-opts (fo/add-margin fig-opts mleft mtop mright mbottom)]
    (->> children
      (mapv #(merge-fig-opts % new-fig-opts)))))

(defmethod r/render :split [_ fig-opts & children]
  (let [split-points (fo/split fig-opts
                       (:direction fig-opts)
                       (:splits fig-opts))]
    (->> children
      (map-indexed
        (fn [index child]
          (merge-fig-opts child (get split-points index))))
      (vec))))

(defmethod r/render :grid [_ fig-opts & children]
  (let [cells-fig-opts (fo/grid fig-opts)]
    (->> cells-fig-opts
      (mapcat (fn [cell-fig-opts]
                (->> children
                  (mapv #(merge-fig-opts % fig-opts cell-fig-opts))))))))

;; TODO: add link-type to change PDPageFitWidthDestination
(defmethod r/render :page-link [_ {:keys [target-page x1 y1 x2 y2] :as fig-opts} & children]
  (let [target-page (cond
                      (string? target-page)
                      (->> r/*all-pages*
                        (filter (fn [page]
                                  (or (= target-page (:name page))
                                    (some #(= target-page %) (:aliases page)))))
                        (first)
                        (:page-obj))

                      :else
                      target-page)
        annotations (.getAnnotations r/*page*)
        annotation-link (PDAnnotationLink.)
        width (abs (- x2 x1))
        height (abs (- y2 y1))
        rect (PDRectangle. x1 y1 width height)
        go-to-action (PDActionGoTo.)
        destination (PDPageFitWidthDestination.)
        border-style (doto (PDBorderStyleDictionary.)
                       (.setWidth 0))]
    (.setRectangle annotation-link rect)
    (.setPage destination target-page)
    (.setDestination go-to-action destination)
    (.setAction annotation-link go-to-action)
    (.setHighlightMode annotation-link PDAnnotationLink/HIGHLIGHT_MODE_NONE)
    (.setBorderStyle annotation-link border-style)
    (.add annotations annotation-link))
  children)

;; TODO: add align option
(defmethod r/render :aligned-pattern-wrapper [_ fig-opts & children]
  (into [:margin (fo/aligned-pattern-wrapper fig-opts)]
    children))

;; TODO: add draw-order attrs (to draw row lines over col lines)
(defmethod r/render :pattern-grid [_ {:as fig-opts
                                      :keys [pattern x1 y1 x2 y2]}
                                   & children]
  (let [aligned-fig-opts (->> (assoc fig-opts
                                :horizontal-align :center
                                :vertical-align :center)
                           (fo/aligned-pattern-wrapper)
                           (fo/add-margin fig-opts))
        {:keys [cell line outline row col]} pattern
        {:keys [cells lines outlines rows cols]} (fo/pattern-grid aligned-fig-opts)]
    [:div
     (when cell
       (->> cells
         (map #(merge-fig-opts cell %))
         (into [:div])))
     (when line
       (->> lines
         (map #(merge-fig-opts line %))
         (into [:div])))
     (when outline
       (->> outlines
         (map #(merge-fig-opts outline % {:cap-style 2}))
         (into [:div])))
     (when row
       (->> rows
         (map #(merge-fig-opts row % {:rows rows}))
         (into [:div])))
     (when col
       (->> cols
         (map #(merge-fig-opts col % {:cols cols}))
         (into [:div])))]))
