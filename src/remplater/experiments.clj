(ns remplater.experiments
  (:import
    [org.apache.pdfbox.pdmodel PDDocument PDPage PDResources PDPageContentStream PDPageContentStream$AppendMode PDPatternContentStream]
    [org.apache.pdfbox.pdmodel.graphics.pattern PDTilingPattern]
    [org.apache.pdfbox.pdmodel.graphics.color PDPattern PDColor PDDeviceRGB]
    [org.apache.pdfbox.pdmodel.common PDRectangle]
    [java.awt Color]))

(comment
  (let [doc (PDDocument.)
        rm2-size (PDRectangle. 1404 1872)
        page (PDPage. rm2-size)]
    (.addPage doc page)
    (.setResources page (PDResources.))

    #_(with-open [cs (PDPageContentStream. doc page PDPageContentStream$AppendMode/APPEND true)]
        (.setNonStrokingColor cs Color/ORANGE)
        (.setLineWidth cs 0)
        (.addRect cs 0 0 104 1872)
        (.fill cs))

    #_(with-open [cs (PDPageContentStream. doc page PDPageContentStream$AppendMode/APPEND true)]
        (.setStrokingColor cs Color/ORANGE)
        (.setLineWidth cs 1)
        (.moveTo cs 105 0)
        (.lineTo cs 105 1872)
        (.fillAndStroke cs))

    #_(with-open [cs (PDPageContentStream. doc page PDPageContentStream$AppendMode/APPEND true)]
        (.setNonStrokingColor cs Color/ORANGE)
        (.setLineWidth cs 1)
        (.addRect cs 100 100 200 200)
        (.stroke cs))

    (with-open [cs (PDPageContentStream. doc page PDPageContentStream$AppendMode/APPEND true)]
      (let [pattern-cs1 (PDPattern. nil PDDeviceRGB/INSTANCE)
            tiling-pattern (PDTilingPattern.)]
        (.setBBox tiling-pattern (PDRectangle. 0 0 10 10))
        (.setPaintType tiling-pattern PDTilingPattern/PAINT_COLORED)
        (.setTilingType tiling-pattern PDTilingPattern/TILING_CONSTANT_SPACING)
        (.setXStep tiling-pattern 10)
        (.setYStep tiling-pattern 10)

        (let [pattern-name-1 (.add (.getResources page) tiling-pattern)]
          (with-open [cs2 (PDPatternContentStream. tiling-pattern)]
            (.setStrokingColor cs2 Color/RED)
            (.moveTo cs2 0 0)
            (.lineTo cs2 10 10)
            (.moveTo cs2 -1 9)
            (.lineTo cs2 1 11)
            (.moveTo cs2 9 -1)
            (.lineTo cs2 11 1)
            (.stroke cs2))

          (let [color-1 (PDColor. pattern-name-1 pattern-cs1)]
            (.setNonStrokingColor cs color-1)
            (.addRect cs 50 500 200 200)
            (.fill cs)))))

    (.save doc "/tmp/blank.pdf")))
