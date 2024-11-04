(ns remplater.templates.experiments
  (:require
    [remplater.components :as c]
    [remplater.fig-operations :as fo]
    [remplater.pdf :as pdf]))

(defn- generate-page-map [document]
  (comment (keys (generate-page-map nil)))

  (let [page-names (->> [(for [m (range 1 6)]
                           (str "month-" m))]
                     (filter some?)
                     (flatten))]
    (->> page-names
      (reduce (fn [page-map current-page-name]
                (assoc page-map current-page-name "123"))
        {}))))

(defn calendar-page [docuemnt page pcs fig-opts]
  (fo/grid (assoc fig-opts :rows 3 :cols 3)
    (fn [fig-opts]
      (c/rect fig-opts))))

(defn simple-planner [document page pcs]
  (let [fig-opts (-> page
                   (pdf/page->pdrect)
                   (pdf/pdrect->fig-opts)
                   (assoc :cs pcs))]
    (calendar-page document page pcs fig-opts)))

(comment
  (pdf/with-document "/tmp/blank.pdf"
    (fn [doc]
      (let [page-1 (pdf/make-page doc)
            page-2 (pdf/make-page doc)]

        (pdf/with-page-content-stream doc page-1
          (fn [cs]
            (simple-planner doc page-1 cs)))

        (pdf/with-page-content-stream doc page-2
          (fn [cs]))))))
