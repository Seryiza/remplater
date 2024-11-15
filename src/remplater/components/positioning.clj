(ns remplater.components.positioning
  (:require
    [clojure.string :as str])
  (:import
    [org.apache.pdfbox.pdmodel.common PDRectangle]))

(defn percent-units? [units]
  (and
    (string? units)
    (not (str/blank? units))
    (str/ends-with? units "%")))

(defn percent-units->number [units]
  (let [number (str/replace units "%" "")]
    (/ (Integer/parseInt number) 100)))

(defn ->abs-unit [total-size units]
  (if (percent-units? units)
    (* total-size (percent-units->number units))
    units))

(defn attrs->sizes [{:keys [x1 y1 x2 y2]}]
  (let [width (abs (- x2 x1))
        height (abs (- y2 y1))]
    [width height]))

(defn attrs->pdrect [{:as attrs :keys [x1 y1]}]
  (let [[width height] (attrs->sizes attrs)]
    (PDRectangle. x1 y1 width height)))

(defn- split-one [attrs coordinate split-size]
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

;; TODO: add option to ignore borders and ~1px empty space between figs
(defn- join-two [attrs-1 attrs-2]
  (let [left-bottom-order?
        (and (<= (:x1 attrs-1) (:x1 attrs-2))
          (<= (:y1 attrs-1) (:y1 attrs-2)))

        [left-bottom-el right-top-el]
        (if left-bottom-order?
          [attrs-1 attrs-2]
          [attrs-2 attrs-1])

        can-join?
        (or
          (and
            (= (:x1 left-bottom-el) (:x1 right-top-el))
            (= (:x2 left-bottom-el) (:x2 right-top-el))
            (= (:y2 left-bottom-el) (:y1 right-top-el)))
          (and
            (= (:y1 left-bottom-el) (:y1 right-top-el))
            (= (:y2 left-bottom-el) (:y2 right-top-el))
            (= (:x2 left-bottom-el) (:x1 right-top-el))))

        joined
        {:x1 (:x1 left-bottom-el)
         :y1 (:y1 left-bottom-el)
         :x2 (:x2 right-top-el)
         :y2 (:y2 right-top-el)}]
    (when can-join?
      joined)))

(defn join [& attrs-to-join]
  (when (not-empty attrs-to-join)
    (->> attrs-to-join
      (rest)
      (reduce (fn [joined-attrs curr-attrs]
                (join-two joined-attrs curr-attrs))
        (first attrs-to-join)))))

(defn margin
  ([attrs margin-units]
   (if (number? margin-units)
     (margin attrs margin-units margin-units margin-units margin-units)
     (margin attrs
       (or (:margin margin-units) (:margin-left margin-units) 0)
       (or (:margin margin-units) (:margin-top margin-units) 0)
       (or (:margin margin-units) (:margin-right margin-units) 0)
       (or (:margin margin-units) (:margin-bottom margin-units) 0))))

  ([attrs margin-vertical margin-horizontal]
   (margin attrs margin-horizontal margin-vertical margin-horizontal margin-vertical))

  ([attrs margin-left margin-top margin-right margin-bottom]
   (let [[width height] (attrs->sizes attrs)
         margin-left (->abs-unit width margin-left)
         margin-right (->abs-unit width margin-right)
         margin-top (->abs-unit height margin-top)
         margin-bottom (->abs-unit height margin-bottom)]
     (-> attrs
       (update :x1 + margin-left)
       (update :x2 - margin-right)
       (update :y1 + margin-bottom)
       (update :y2 - margin-top)))))

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

(defn rect->middle-line [{:as attrs :keys [line-type x1 y1 x2 y2]}]
  (let [{:keys [x y]} (rect->center attrs)]
    (case line-type
      :horizontal-middle
      (assoc attrs :y1 y :y2 y)

      :vertical-middle
      (assoc attrs :x1 x :x2 x)

      nil)))

(defn grid [attrs]
  (let [{:keys [x1 y1 x2 y2 rows cols cell-size]} attrs
        [width height] (attrs->sizes attrs)
        x-delta (/ width cols)
        y-delta (/ height rows)
        splits-x (repeat (dec cols) x-delta)
        splits-y (repeat (dec rows) y-delta)
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
        cells (->> (split attrs :y splits-y)
                (map #(split % :x splits-x))
                (flatten)
                (map-indexed #(assoc %2 :index %1)))]
    {:rows rows
     :cols cols
     :lines (concat lines-y lines-x)
     :outlines outlines
     :cells cells}))

(defn pattern-grid [{:as attrs :keys [pattern x1 y1 x2 y2]}]
  (let [[width height] (attrs->sizes attrs)
        pattern-width (or (->abs-unit width (:width pattern))
                        width)
        pattern-height (or (->abs-unit height (:height pattern))
                         height)
        used-patterns-x (quot width pattern-width)
        used-patterns-y (quot height pattern-height)
        {:keys [max-cols max-rows]} pattern
        used-patterns-x (if max-cols
                          (min max-cols used-patterns-x)
                          used-patterns-x)
        used-patterns-y (if max-rows
                          (min max-rows used-patterns-y)
                          used-patterns-y)
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
        cells (:cells (grid (assoc attrs
                              :rows used-patterns-y
                              :cols used-patterns-x)))]
    {:cells cells
     :lines (concat lines-y lines-x)
     :rows rows
     :cols cols
     :outlines outlines}))

;; TODO: add align option
(defn aligned-pattern-wrapper [{:as attrs :keys [x1 y1 x2 y2 pattern
                                                 horizontal-align vertical-align]}]
  (let [[box-width box-height] (attrs->sizes attrs)
        pattern-width (or (->abs-unit box-width (:width pattern))
                        box-width)
        pattern-height (or (->abs-unit box-height (:height pattern))
                         box-height)
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
