(ns remplater.components.builtin-components
  (:require
    [remplater.components.positioning :as pos]
    [remplater.components.render :as r]
    [remplater.pdf :as pdf])

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

(defmethod r/render :div
  [_ attrs & children]
  children)

(defmethod r/render :rect
  [_ {:keys [fill? stroke? fill-color line-width x1 y1 x2 y2]
      :or {fill? true
           stroke? false
           line-width 1.0}
      :as attrs}
   & children]
  (let [[width height] (pos/attrs->sizes attrs)
        fill-color (cond
                     (fn? fill-color) (fill-color attrs)
                     (some? fill-color) fill-color
                     :else (pdf/make-color-by-attrs attrs))]
    (doto r/*cs*
      (.setNonStrokingColor fill-color)
      (.setLineWidth line-width)
      (.addRect x1 y1 width height))

    (cond
      (and fill? stroke?) (.fillAndStroke r/*cs*)
      fill? (.fill r/*cs*)
      stroke? (.stroke r/*cs*))

    (.closePath r/*cs*)
    children))

(defmethod r/render :circle
  [_ {:as attrs :keys [radius fill-color]} & children]
  (let [{:keys [x y]} (pos/rect->center attrs)]
    (doto r/*cs*
      (.setNonStrokingColor fill-color)
      (pdf/draw-circle x y radius)
      (.fill)))
  children)

(defmethod r/render :line
  [_ {:keys [x1 y1 x2 y2 width color cap-style]
      :or {width 1
           cap-style 0
           color Color/BLACK}}
   & children]
  (doto r/*cs*
    (.setLineWidth width)
    (.setStrokingColor color)
    (.moveTo x1 y1)
    (.lineTo x2 y2)
    (.setLineCapStyle cap-style)
    (.stroke))
  children)

(defmethod r/render :border
  [_ {:as attrs :keys [border-left border-top border-right border-bottom]}
   & children]
  (let [make-border-line-fn
        (fn [border-type border-opts]
          [:line (merge attrs (pos/rect->border-line attrs border-type))])]
    (->> [(when border-left
            (make-border-line-fn :left border-left))
          (when border-right
            (make-border-line-fn :right border-right))
          (when border-top
            (make-border-line-fn :top border-top))
          (when border-bottom
            (make-border-line-fn :bottom border-bottom))]
      (filter some?)
      (vec))))

(defmethod r/render :text
  [_ {:as attrs :keys [x1 y1 x2 y2 halign valign text font font-size fill-color text-offset children-offset]
      :or {font-size 12
           fill-color Color/BLACK
           valign :top
           halign :left
           children-offset 0}}
   & children]
  (let [font (or
               (r/get-attr r/*document* :fonts font)
               (r/get-attr r/*document* :fonts :default)
               (PDType1Font. Standard14Fonts$FontName/HELVETICA))]
    (let [[block-width block-height] (pos/attrs->sizes attrs)
          text-height font-size
          text-width (* font-size (/ (.getStringWidth font text) 1000))
          text-offset (or text-offset (/ text-height 6))
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

          child-attrs {:x1 text-pos-x
                       :y1 (+ text-pos-y children-offset)
                       :x2 (+ text-pos-x text-width)
                       :y2 (+ text-pos-y text-height children-offset)}]
      (if (not-empty children)
        ;; TODO: make it as more elegant way?
        (->> (concat children [[:text attrs]])
          (mapv #(r/merge-unexisting-attrs % child-attrs)))

        (doto r/*cs*
          (.beginText)
          (.setNonStrokingColor fill-color)
          (.setFont font font-size)
          (.newLineAtOffset text-pos-x (+ text-pos-y text-offset))
          (.showText text)
          (.endText))))))

(defmethod r/render :margin
  [_ attrs & children]
  (let [mleft (or (:margin-left attrs) (:margin attrs) 0)
        mtop (or (:margin-top attrs) (:margin attrs) 0)
        mright (or (:margin-right attrs) (:margin attrs) 0)
        mbottom (or (:margin-bottom attrs) (:margin attrs) 0)
        new-attrs (pos/margin attrs mleft mtop mright mbottom)]
    (->> children
      (mapv #(r/merge-unexisting-attrs % new-attrs)))))

(defmethod r/render :join
  [_ attrs & children]
  (let [{:keys [joins]} attrs
        joined-attrs (apply pos/join joins)]
    (when joins
      (->> children
        (mapv #(r/merge-unexisting-attrs % joined-attrs))))))

(defmethod r/render :split
  [_ {:as attrs :keys [direction splits]} & children]
  (let [split-attrs (pos/split attrs direction splits)]
    (->> children
      (map-indexed
        (fn [index child]
          (r/merge-unexisting-attrs child (get split-attrs index))))
      (vec))))

(defmethod r/render :grid
  [_ attrs & children]
  (let [cells-attrs (pos/grid attrs)]
    (->> cells-attrs
      (mapcat (fn [cell-attrs]
                (->> children
                  (mapv #(r/merge-unexisting-attrs % cell-attrs attrs))))))))

;; TODO: add link-type to change PDPageFitWidthDestination
(defmethod r/render :page-link
  [_ {:as attrs :keys [target-page x1 y1 x2 y2]} & children]
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
        rect (pos/attrs->pdrect attrs)
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

(defmethod r/render :aligned-pattern-wrapper
  [_ attrs & children]
  (into [:margin (pos/aligned-pattern-wrapper attrs)]
    children))

;; TODO: add draw-order attrs (to draw row lines over col lines)
(defmethod r/render :pattern-grid
  [_ {:as attrs :keys [pattern x1 y1 x2 y2]}
   & children]
  (let [aligned-attrs (->> (assoc attrs
                             :horizontal-align :center
                             :vertical-align :center)
                           (pos/aligned-pattern-wrapper)
                           (pos/margin attrs))
        {:keys [cell line outline row col]} pattern
        {:keys [cells lines outlines rows cols]} (pos/pattern-grid aligned-attrs)]
    [:div
     (when cell
       (->> cells
         (map #(r/merge-unexisting-attrs cell % attrs))
         (into [:div])))
     (when line
       (->> lines
         (map #(r/merge-unexisting-attrs line % attrs))
         (into [:div])))
     (when outline
       (->> outlines
         (map #(r/merge-unexisting-attrs outline % {:cap-style 2} attrs))
         (into [:div])))
     (when row
       (->> rows
         (map #(r/merge-unexisting-attrs row % {:rows rows} attrs))
         (into [:div])))
     (when col
       (->> cols
         (map #(r/merge-unexisting-attrs col % {:cols cols} attrs))
         (into [:div])))]))
