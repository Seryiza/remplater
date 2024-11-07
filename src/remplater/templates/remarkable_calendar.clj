(ns remplater.templates.remarkable-calendar
  (:require
    [remplater.components :as c]
    [remplater.fig-operations :as fo]
    [remplater.patterns :as patterns]
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

(defn monthly-page [{:keys [year month]}]
  (let [days (get-monthly-days year month)]
    [c/page {:size pdf/remarkable-2-horizontal-page-size
             :name (str "monthly-page-" year "-" month)}
     [c/margin {:margin 80
                :margin-top 50}
      [c/split {:direction :y :splits [200]}

       ;; header
       [c/margin {:margin-bottom 20}
        [c/split {:direction :x :splits [#(/ % 7)]}
         [c/div {}
          [c/border {:border-right true
                     :width 4}]
          [c/split {:direction :y :splits [50]}
           [c/text {:text "month num"}]
           [c/text {:text "year num"}]]]
         [c/div {}
          [c/text {:text "month name"}]]]]

       ;; days grid
       [c/grid {:rows 5 :cols 7}
        (fn [fig-opts]
          [[c/page-link {:target-page "day-01"}]
           [c/rect {:fill? false
                    :stroke? true
                    :line-width 4}]
           [c/margin {:margin-top 10
                      :margin-left 20}
            [c/text {:text (get days (:index fig-opts))
                     :font-size 40}]]])]]]]))

(defn daily-layout [{:keys [year month day]}]
  [c/aligned-pattern-wrapper (assoc patterns/cells
                               :horizontal-align :center)
   [c/split {:direction :y :splits [100]}
    [c/split {:direction :x :splits [100]}
     [c/text {:text "27"}]
     [c/split {:direction :y :splits [#(/ % 2)]}
      [c/text {:text "friday"}]
      [c/text {:text "september"}]]]
    [c/pattern-grid patterns/cells]]])

(defn daily-page [{:as opts :keys [year month day]}]
  [c/page {:name (str "daily-page-" year "-" month "-" day)
           :size pdf/remarkable-2-horizontal-page-size}
   [c/margin {:margin 80
              :margin-top 50}
    [c/split {:direction :x
              :splits [#(/ % 2)]}
     [c/margin {:margin-right 20}
      [daily-layout opts]]
     [c/margin {:margin-left 20}
      [daily-layout opts]]]]])

(comment
  (render/render-document
    (into [c/document {:output "/tmp/remarkable_calendar.pdf"}]
      (concat
        #_(->> (range 1 13)
            (mapv (fn [month]
                    [monthly-page {:year 2024 :month month}])))

        [[daily-page {:year 2024 :month 9 :day 1}]]))))
