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

  (->> splits
    (reduce (fn [all next-split]
              (let [splitted-pair (split-one
                                    (last all)
                                    coordinate
                                    next-split)]
                (-> all
                  (drop-last)
                  (concat splitted-pair))))
      [fig-opts])))

(defn add-margin [fig-opts margin & [margin-horizontal]]
  (-> fig-opts
    (update :x1 + (or margin-horizontal margin))
    (update :x2 - (or margin-horizontal margin))
    (update :y1 + margin)
    (update :y2 - margin)))

(defn grid [fig-opts f]
  (let [{:keys [x1 y1 x2 y2 rows cols]} fig-opts
        x-delta (/ (- x2 x1) cols)
        y-delta (/ (- y2 y1) rows)
        x-slices (repeat (dec cols) x-delta)
        y-slices (repeat (dec rows) y-delta)
        cells (->> (split fig-opts :y y-slices)
                (map #(split % :x x-slices))
                (flatten)
                (map-indexed #(assoc %2 :index %1)))]
    (->> cells
      (map #(f %))
      (doall))))
