(ns remplater.templates.experiments
  (:require
    [remplater.components :as c]
    [remplater.fig-operations :as fo]
    [remplater.pdf :as pdf]))

(defn- calendar-page [document page pcs fig-opts]
  (let [[sidebar-fig content-fig]
        (fo/split fig-opts :x [104])

        [navbar-fig header-fig calendar-fig empty-fig]
        (fo/split content-fig :y [104 300 1000])]

    (->> [sidebar-fig
          navbar-fig
          #_#_#_header-fig
              calendar-fig
            empty-fig]
      (map #(c/rect pcs %))
      (doall))

    (fo/grid (-> empty-fig
               (assoc :rows 4 :cols 3)
               (fo/add-margin 50))
      (fn [fig-opts]
        (c/rect pcs fig-opts)))

    (fo/grid (-> calendar-fig
               (assoc :rows 5 :cols 7)
               (fo/add-margin 50))
      (fn [{:keys [index] :as fig-opts}]
        (let [day (if (>= index 10)
                    (str index)
                    (str "0" index))
              rect-opts (-> fig-opts
                          (assoc :stroke? true)
                          (assoc :fill? false)
                          (assoc :line-width 5)
                          (fo/add-margin 0))
              text-opts (-> fig-opts
                          (assoc :font-size 35)
                          (assoc :text day)
                          (fo/add-margin 5 12))
              link-opts (-> fig-opts
                          (assoc :target-page (pdf/get-document-page document 1)))]
          (c/rect pcs rect-opts)
          (c/text pcs text-opts)
          (c/page-link {:page page :cs pcs} link-opts))))))

(defn simple-planner [document page pcs]
  (let [fig-opts (-> page
                   (pdf/page->pdrect)
                   (pdf/pdrect->fig-opts))]
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
