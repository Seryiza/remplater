(ns remplater.fig-operations)

(defn split-one [fig-opts coordinate split-size]
  (comment
    (split-one {:x1 100 :y1 100 :x2 200 :y2 200} :x #(/ % 3))
    (split-one {:x1 100 :y1 100 :x2 200 :y2 200} :x 20)
    (split-one {:x1 100 :y1 100 :x2 200 :y2 200} :y 20))

  (let [horizontal? (= :x coordinate)
        [lower-kw upper-kw] (if horizontal?
                              [:x1 :x2]
                              [:y1 :y2])
        lower (get fig-opts lower-kw)
        upper (get fig-opts upper-kw)
        total-size (abs (- upper lower))
        size-delta (if (fn? split-size)
                     (split-size total-size)
                     split-size)
        splitted-coordinate (if horizontal?
                              (+ lower size-delta)
                              (- upper size-delta))
        fig-opts-1 (-> fig-opts
                     (assoc upper-kw splitted-coordinate))
        fig-opts-2 (-> fig-opts
                     (assoc lower-kw splitted-coordinate))]
    (if horizontal?
      [fig-opts-1 fig-opts-2]
      [fig-opts-2 fig-opts-1])))

(defn split [fig-opts coordinate splits]
  (comment
    (split {:x1 100 :y1 100 :x2 200 :y2 200} :y [10 20]))

  (let [fig-opts (select-keys fig-opts [:x1 :y1 :x2 :y2])]
    (->> splits
      (reduce (fn [all next-split]
                (let [splitted-pair (split-one
                                      (last all)
                                      coordinate
                                      next-split)]
                  (-> all
                    (drop-last)
                    (concat splitted-pair))))
        [fig-opts])
      (vec))))

(defn add-margin
  ([fig-opts margin]
   (if (map? margin)
     (add-margin fig-opts (:margin-left margin) (:margin-top margin) (:margin-right margin) (:margin-bottom margin))
     (add-margin fig-opts margin margin margin margin)))

  ([fig-opts margin-vertical margin-horizontal]
   (add-margin fig-opts margin-horizontal margin-vertical margin-horizontal margin-vertical))

  ([fig-opts margin-left margin-top margin-right margin-bottom]
   (-> fig-opts
     (update :x1 + margin-left)
     (update :x2 - margin-right)
     (update :y1 + margin-bottom)
     (update :y2 - margin-top))))

(defn grid [fig-opts]
  (let [{:keys [x1 y1 x2 y2 rows cols]} fig-opts
        x-delta (/ (- x2 x1) cols)
        y-delta (/ (- y2 y1) rows)
        x-slices (repeat (dec cols) x-delta)
        y-slices (repeat (dec rows) y-delta)
        cells (->> (split fig-opts :y y-slices)
                (map #(split % :x x-slices))
                (flatten)
                (map-indexed #(assoc %2 :index %1)))]
    cells))

(defn rect->border-line [fig-opts side]
  (let [{:keys [x1 y1 x2 y2]} fig-opts
        line (case side
               :top [x1 y2 x2 y2]
               :bottom [x1 y1 x2 y1]
               :left [x1 y1 x1 y2]
               :right [x2 y1 x2 y2])]
    {:x1 (get line 0)
     :y1 (get line 1)
     :x2 (get line 2)
     :y2 (get line 3)}))

(defn rect->border-lines [fig-opts]
  {:top (rect->border-line fig-opts :top)
   :bottom (rect->border-line fig-opts :bottom)
   :left (rect->border-line fig-opts :left)
   :right (rect->border-line fig-opts :right)})

(defn rect->center [{:as fig-opts :keys [x1 y1 x2 y2]}]
  (let [width (abs (- x2 x1))
        height (abs (- y2 y1))]
    {:x (+ x1 (/ width 2))
     :y (+ y1 (/ height 2))}))

(defn pattern-grid [{:as fig-opts :keys [pattern x1 y1 x2 y2]}]
  (let [width (abs (- x2 x1))
        height (abs (- y2 y1))
        pattern-width (:width pattern)
        pattern-height (:height pattern)
        used-patterns-x (quot width pattern-width)
        used-patterns-y (quot height pattern-height)
        splits-x (vec (repeat (dec used-patterns-x) pattern-width))
        splits-y (vec (repeat (dec used-patterns-y) pattern-height))
        lines-x (->> (split fig-opts :x splits-x)
                  (map #(rect->border-line % :right))
                  (map-indexed #(assoc %2 :col-index %1))
                  (drop-last))
        lines-y (->> (split fig-opts :y splits-y)
                  (map #(rect->border-line % :bottom))
                  (map-indexed #(assoc %2 :row-index %1))
                  (drop-last))
        outlines (vals (rect->border-lines fig-opts))
        cells (grid (assoc fig-opts
                      :rows used-patterns-y
                      :cols used-patterns-x))]
    {:cells cells
     :lines (concat lines-y lines-x)
     :outlines outlines}))

(defn aligned-pattern-wrapper [{:as fig-opts
                                :keys [x1 y1 x2 y2 pattern
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
