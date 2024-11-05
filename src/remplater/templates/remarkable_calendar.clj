(ns remplater.templates.remarkable-calendar
  (:require
    [remplater.components :as c]
    [remplater.fig-operations :as fo]
    [remplater.pdf :as pdf]
    [remplater.render :as render]
    [tick.core :as t]))

(defn get-monthly-days [year month]
  (let [month-start (t/new-date year month 1)
        calendar-start-day (t/previous-or-same month-start t/MONDAY)
        day-formatter (t/formatter "dd")]
    (->> (range 35)
      (mapv (fn [day-offset]
              (t/format
                day-formatter
                (t/>> calendar-start-day (t/of-days day-offset))))))))

(defn monthly-header [fig-opts]
  [[c/margin {:margin-bottom 20}
    [c/split {:direction :x :splits [#(/ % 7)]}
     [c/div {}
      [c/border {:border-right true
                 :width 4}]
      [c/split {:direction :y :splits [50]}
       [c/text {:text "month num"}]
       [c/text {:text "year num"}]]]
     [c/div {}
      [c/text {:text "month name"}]]]]])

(defn monthly-days [{:as fig-opts :keys [year month]}]
  (let [days (get-monthly-days year month)]
    [[c/grid {:rows 5 :cols 7}
      [(fn [fig-opts]
         [[c/page-link {:target-page "day-01"}]
          [c/rect {:fill? false
                   :stroke? true
                   :line-width 4}]
          [c/margin {:margin-top 10
                     :margin-left 20}
           [c/text {:text (get days (:index fig-opts))
                    :font-size 40}]]])]]]))

(render/render-document
  [c/document {:output "/tmp/remarkable_calendar.pdf"}
   [c/page {:size pdf/remarkable-2-horizontal-page-size}
    [c/margin {:margin 80
               :margin-top 50}
     [c/split {:direction :y :splits [200]}
      [monthly-header {}]
      [monthly-days {:year 2024 :month 10}]]]]

   [c/page {:name "day-01"
            :size pdf/remarkable-2-horizontal-page-size}
    [c/rect {:fill-color (pdf/make-color 150 150 150)}]]])
