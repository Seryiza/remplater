(ns remplater.components.positioning
  (:import
    [org.apache.pdfbox.pdmodel.common PDRectangle]))

(defn attrs->sizes [{:keys [x1 y1 x2 y2]}]
  (let [width (abs (- x2 x1))
        height (abs (- y2 y1))]
    [width height]))

(defn attrs->pdrect [{:as attrs :keys [x1 y1]}]
  (let [[width height] (attrs->sizes attrs)]
    (PDRectangle. x1 y1 width height)))

(defn split-one [attrs coordinate split-size]
  (comment
    (split-one {:x1 100 :y1 100 :x2 200 :y2 200} :x #(/ % 3))
    (split-one {:x1 100 :y1 100 :x2 200 :y2 200} :x 20)
    (split-one {:x1 100 :y1 100 :x2 200 :y2 200} :y 20))

  (let [horizontal? (= :x coordinate)
        [lower-kw upper-kw] (if horizontal?
                              [:x1 :x2]
                              [:y1 :y2])
        lower (get attrs lower-kw)
        upper (get attrs upper-kw)
        total-size (abs (- upper lower))
        size-delta (if (fn? split-size)
                     (split-size total-size)
                     split-size)
        splitted-coordinate (if horizontal?
                              (+ lower size-delta)
                              (- upper size-delta))
        attrs-1 (-> attrs
                  (assoc upper-kw splitted-coordinate))
        attrs-2 (-> attrs
                  (assoc lower-kw splitted-coordinate))]
    (if horizontal?
      [attrs-1 attrs-2]
      [attrs-2 attrs-1])))

(defn split [attrs coordinate splits]
  (comment
    (split {:x1 100 :y1 100 :x2 200 :y2 200} :y [10 20]))

  (let [attrs (select-keys attrs [:x1 :y1 :x2 :y2])]
    (->> splits
      (reduce (fn [all next-split]
                (let [splitted-pair (split-one
                                      (last all)
                                      coordinate
                                      next-split)]
                  (-> all
                    (drop-last)
                    (concat splitted-pair))))
        [attrs])
      (vec))))

;; TODO: add option for horizontal join
;; TODO: add option to ignore borders and ~1px empty space between figs
;; TODO: add join fn like split fn
(defn join-two [attrs-1 attrs-2]
  (let [left-bottom-order?
        (and (<= (:x1 attrs-1) (:x1 attrs-2))
          (<= (:y1 attrs-1) (:y1 attrs-2)))

        [left-bottom-el right-top-el]
        (if left-bottom-order?
          [attrs-1 attrs-2]
          [attrs-2 attrs-1])]
    (cond
      (and
        (= (:x1 left-bottom-el) (:x1 right-top-el))
        (= (:x2 left-bottom-el) (:x2 right-top-el))
        (= (:y2 left-bottom-el) (:y1 right-top-el)))
      {:x1 (:x1 left-bottom-el)
       :y1 (:y1 left-bottom-el)
       :x2 (:x2 right-top-el)
       :y2 (:y2 right-top-el)})))

(defn add-margin
  ([attrs margin]
   (if (map? margin)
     (add-margin attrs (:margin-left margin) (:margin-top margin) (:margin-right margin) (:margin-bottom margin))
     (add-margin attrs margin margin margin margin)))

  ([attrs margin-vertical margin-horizontal]
   (add-margin attrs margin-horizontal margin-vertical margin-horizontal margin-vertical))

  ([attrs margin-left margin-top margin-right margin-bottom]
   (-> attrs
     (update :x1 + margin-left)
     (update :x2 - margin-right)
     (update :y1 + margin-bottom)
     (update :y2 - margin-top))))

(defn grid [attrs]
  (let [{:keys [x1 y1 x2 y2 rows cols]} attrs
        x-delta (/ (- x2 x1) cols)
        y-delta (/ (- y2 y1) rows)
        x-slices (repeat (dec cols) x-delta)
        y-slices (repeat (dec rows) y-delta)
        cells (->> (split attrs :y y-slices)
                (map #(split % :x x-slices))
                (flatten)
                (map-indexed #(assoc %2 :index %1)))]
    cells))

(defn rect->border-line [attrs side]
  (let [{:keys [x1 y1 x2 y2]} attrs
        line (case side
               :top [x1 y2 x2 y2]
               :bottom [x1 y1 x2 y1]
               :left [x1 y1 x1 y2]
               :right [x2 y1 x2 y2])]
    (assoc attrs
      :x1 (get line 0)
      :y1 (get line 1)
      :x2 (get line 2)
      :y2 (get line 3))))

(defn rect->border-lines [attrs]
  {:top (rect->border-line attrs :top)
   :bottom (rect->border-line attrs :bottom)
   :left (rect->border-line attrs :left)
   :right (rect->border-line attrs :right)})

(defn rect->center [{:as attrs :keys [x1 y1 x2 y2]}]
  (let [[width height] (attrs->sizes attrs)]
    {:x (+ x1 (/ width 2))
     :y (+ y1 (/ height 2))}))

(defn pattern-grid [{:as attrs :keys [pattern x1 y1 x2 y2]}]
  (let [[width height] (attrs->sizes attrs)
        pattern-width (:width pattern)
        pattern-height (:height pattern)
        used-patterns-x (quot width pattern-width)
        used-patterns-y (quot height pattern-height)
        splits-x (vec (repeat (dec used-patterns-x) pattern-width))
        splits-y (vec (repeat (dec used-patterns-y) pattern-height))
        cols (->> (split attrs :x splits-x)
               (map-indexed #(assoc %2 :col-index %1))
               (vec))
        lines-x (->> cols
                  (map #(rect->border-line % :right))
                  (drop-last))
        rows (->> (split attrs :y splits-y)
               (map-indexed #(assoc %2 :row-index %1))
               (vec))
        lines-y (->> rows
                  (map #(rect->border-line % :bottom))
                  (drop-last))
        outlines (vals (rect->border-lines attrs))
        cells (grid (assoc attrs
                      :rows used-patterns-y
                      :cols used-patterns-x))]
    {:cells cells
     :lines (concat lines-y lines-x)
     :rows rows
     :cols cols
     :outlines outlines}))

;; TODO: add align option
(defn aligned-pattern-wrapper [{:keys [x1 y1 x2 y2 pattern
                                       horizontal-align vertical-align]}]
  (let [box-width (abs (- x2 x1))
        box-height (abs (- y2 y1))
        pattern-width (:width pattern)
        pattern-height (:height pattern)
        used-patterns-x (quot box-width pattern-width)
        used-patterns-y (quot box-height pattern-height)
        used-space-x (* pattern-width used-patterns-x)
        used-space-y (* pattern-height used-patterns-y)
        free-space-x (- box-width used-space-x)
        free-space-y (- box-height used-space-y)

        horizontal-margin-opts
        (case horizontal-align
          :center {:margin-left (/ free-space-x 2)
                   :margin-right (/ free-space-x 2)}
          {})

        vertical-margin-opts
        (case vertical-align
          :center {:margin-top (/ free-space-y 2)
                   :margin-bottom (/ free-space-y 2)}
          {})]
    (merge horizontal-margin-opts
      vertical-margin-opts)))
