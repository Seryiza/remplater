(ns remplater.patterns
  (:require
    [remplater.components :as c]
    [remplater.pdf :as pdf]
    [remplater.render :as render])
  (:import
    [org.apache.pdfbox.pdmodel PDDocument PDPage PDResources PDPageContentStream PDPageContentStream$AppendMode PDPatternContentStream]
    [org.apache.pdfbox.pdmodel.graphics.pattern PDTilingPattern]
    [org.apache.pdfbox.pdmodel.graphics.color PDPattern PDColor PDDeviceRGB]
    [org.apache.pdfbox.pdmodel.common PDRectangle]
    [java.awt Color]))

(def gray-line
  [c/line {:color (pdf/make-color 100 100 100)}])

(defn calc-offset [{:as fig-opts :keys [x1 y1 x2 y2 width height]}]
  (let [fig-width (abs (- x2 x1))
        fig-height (abs (- y2 y1))]
    {:ox (rem x1 width)
     :oy (rem y1 height)}))

(defn create-pattern [{:as fig-opts :keys [width height pattern-fn]}]
  (let [pattern (PDPattern. nil PDDeviceRGB/INSTANCE)
        tiling-pattern (doto (PDTilingPattern.)
                         (.setBBox (PDRectangle. -100 -100 200 200))
                         (.setPaintType PDTilingPattern/PAINT_COLORED)
                         (.setTilingType PDTilingPattern/TILING_CONSTANT_SPACING)
                         (.setXStep width)
                         (.setYStep height))
        pattern-name (-> render/*page*
                       (.getResources)
                       (.add tiling-pattern))
        {:keys [ox oy]} (calc-offset fig-opts)]
    (prn "!ox" ox oy)
    (with-open [cs (PDPatternContentStream. tiling-pattern)]
      (pattern-fn (assoc fig-opts :cs cs :ox ox :oy oy)))

    (PDColor. pattern-name pattern)))

(defn example []
  (create-pattern
    {:width 10
     :height 10
     :pattern-fn (fn [{:keys [cs ox oy]}]
                   (doto cs
                     (.setStrokingColor Color/RED)
                     (.moveTo 0 0)
                     (.lineTo 10 10)
                     (.moveTo -1 9)
                     (.lineTo 1 11)
                     (.moveTo 9 -1)
                     (.lineTo 11 1)
                     (.stroke)))}))

(defn dots [fig]
  (create-pattern
    (assoc fig
      :width 30
      :height 30
      :pattern-fn (fn [{:as fig :keys [cs ox oy]}]
                    (doto cs
                      (.setNonStrokingColor Color/BLACK)
                      (pdf/draw-circle (+ ox 0) (+ oy 0) 2)
                      (.fill))))))

(def cells-pattern
  {:width 45
   :height 45
   :line gray-line
   :outline gray-line})

(def dots
  {:width 30
   :height 30
   :outline [c/line]
   :cell [c/circle {:fill-color (pdf/make-color 100 100 100)
                    :radius 2}]})
