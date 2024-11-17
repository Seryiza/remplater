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
  (cond
    (fn? units) (units total-size)
    (percent-units? units) (* total-size (percent-units->number units))
    (ratio? units) (* total-size units)
    :else units))

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
        size-delta (->abs-unit total-size split-size)
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

(defn padding
  ([attrs padding-units]
   (if (number? padding-units)
     (padding attrs padding-units padding-units padding-units padding-units)
     (padding attrs
       (or (:padding padding-units) (:padding-left padding-units) 0)
       (or (:padding padding-units) (:padding-top padding-units) 0)
       (or (:padding padding-units) (:padding-right padding-units) 0)
       (or (:padding padding-units) (:padding-bottom padding-units) 0))))

  ([attrs padding-vertical padding-horizontal]
   (padding attrs padding-horizontal padding-vertical padding-horizontal padding-vertical))

  ([attrs padding-left padding-top padding-right padding-bottom]
   (let [[width height] (attrs->sizes attrs)
         padding-left (->abs-unit width padding-left)
         padding-right (->abs-unit width padding-right)
         padding-top (->abs-unit height padding-top)
         padding-bottom (->abs-unit height padding-bottom)]
     (-> attrs
       (update :x1 + padding-left)
       (update :x2 - padding-right)
       (update :y1 + padding-bottom)
       (update :y2 - padding-top)))))

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
      :y2 (get line 3)
      :side side)))

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
  (let [{:keys [x1 y1 x2 y2 rows cols]} attrs
        [width height] (attrs->sizes attrs)
        x-delta (/ width cols)
        y-delta (/ height rows)
        splits-x (repeat (dec cols) x-delta)
        splits-y (repeat (dec rows) y-delta)
        cols-attrs (->> (split attrs :x splits-x)
                     (map-indexed #(assoc %2 :col-index %1))
                     (vec))
        cols-attrs (->> cols-attrs
                     (mapv #(assoc % :cols cols-attrs)))
        lines-x (->> cols-attrs
                  (map #(rect->border-line % :right))
                  (drop-last))
        rows-attrs (->> (split attrs :y splits-y)
                     (map-indexed #(assoc %2 :row-index %1))
                     (vec))
        rows-attrs (->> rows-attrs
                     (mapv #(assoc % :rows rows-attrs)))
        lines-y (->> rows-attrs
                  (map #(rect->border-line % :bottom))
                  (drop-last))
        outlines (vals (rect->border-lines attrs))
        cells (->> (split attrs :y splits-y)
                (map #(split % :x splits-x))
                (flatten)
                (map-indexed #(assoc %2 :index %1)))]
    {:rows rows-attrs
     :cols cols-attrs
     :lines (concat lines-y lines-x)
     :outlines outlines
     :cells cells}))

(defn align-according-to-pattern
  [{:as attrs :keys [x1 y1 x2 y2 pattern horizontal-align vertical-align]
    :or {horizontal-align :left
         vertical-align :top}}]
  (let [[box-width box-height] (attrs->sizes attrs)
        pattern-width (or (->abs-unit box-width (:width pattern))
                        box-width)
        pattern-height (or (->abs-unit box-height (:height pattern))
                         box-height)
        used-patterns-x (quot box-width pattern-width)
        used-patterns-y (quot box-height pattern-height)
        {:keys [col-count row-count]} pattern
        used-patterns-x (if col-count
                          (min col-count used-patterns-x)
                          used-patterns-x)
        used-patterns-y (if row-count
                          (min row-count used-patterns-y)
                          used-patterns-y)
        used-space-x (* pattern-width used-patterns-x)
        used-space-y (* pattern-height used-patterns-y)
        free-space-x (- box-width used-space-x)
        free-space-y (- box-height used-space-y)

        horizontal-opts
        (case horizontal-align
          :left {:padding-right free-space-x}
          :center {:padding-left (/ free-space-x 2)
                   :padding-right (/ free-space-x 2)}
          :right {:padding-left free-space-x})

        vertical-opts
        (case vertical-align
          :top {:padding-bottom free-space-y}
          :center {:padding-top (/ free-space-y 2)
                   :padding-bottom (/ free-space-y 2)}
          :bottom {:padding-top free-space-y})]
    (padding attrs (merge horizontal-opts vertical-opts))))

(defn pattern-grid [{:as attrs :keys [pattern x1 y1 x2 y2]}]
  (let [attrs (align-according-to-pattern attrs)
        [width height] (attrs->sizes attrs)
        pattern-width (or (->abs-unit width (:width pattern))
                        width)
        pattern-height (or (->abs-unit height (:height pattern))
                         height)
        used-patterns-x (quot width pattern-width)
        used-patterns-y (quot height pattern-height)]
    (grid (assoc attrs
            :rows used-patterns-y
            :cols used-patterns-x))))
