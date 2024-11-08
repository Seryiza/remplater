(ns remplater.templates.remarkable-calendar
  (:require
    [remplater.components :as c]
    [remplater.fig-operations :as fo]
    [remplater.patterns :as patterns]
    [remplater.pdf :as pdf]
    [remplater.render :as render]
    [tick.core :as t]))

(defn date->units [date]
  (let [year-fmt (t/formatter "YYYY")
        month-fmt (t/formatter "M")
        day-fmt (t/formatter "d")]
    {:year (t/format year-fmt date)
     :month (t/format month-fmt date)
     :day (t/format day-fmt date)}))

(defn get-montly-page-name [date]
  (let [{:keys [year month]} (date->units date)]
    (str "monthly-page-" year "-" month)))

(defn get-daily-page-name [date]
  (let [{:keys [year month day]} (date->units date)]
    (str "daily-page-" year "-" month "-" day)))

(defn range-days [from to & [step]]
  (let [step (or step 1)]
    (->> from
      (iterate #(t/>> % (t/of-days step)))
      (take-while #(t/<= % to)))))

(defn get-monthly-days [month-start]
  (let [calendar-start-day (t/previous-or-same month-start t/MONDAY)
        long-day-formatter (t/formatter "dd")]
    (->> (range 35)
      (map (fn [day-offset]
             (t/>> calendar-start-day (t/of-days day-offset))))
      (mapv (fn [date]
              {:label (t/format long-day-formatter date)
               :this-month? (= (t/month date) (t/month month-start))
               :page-name (get-daily-page-name date)})))))

(defn monthly-page [{:keys [date]}]
  (let [days (get-monthly-days date)]
    [c/page {:size pdf/remarkable-2-horizontal-page-size
             :name (get-montly-page-name date)}
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
          (let [{:keys [label page-name]} (get days (:index fig-opts))]
            [[c/page-link {:target-page page-name}]
             [c/rect {:fill? false
                      :stroke? true
                      :line-width 4}]
             [c/margin {:margin-top 10
                        :margin-left 20}
              [c/text {:text label
                       :font-size 40}]]]))]]]]))

(defn daily-layout [{:keys [date]}]
  [c/aligned-pattern-wrapper {:pattern patterns/cells-pattern
                              :horizontal-align :center}
   [c/split {:direction :y :splits [100]}
    [c/split {:direction :x :splits [100]}
     [c/text {:text "27"}]
     [c/split {:direction :y :splits [#(/ % 2)]}
      [c/text {:text "friday"}]
      [c/text {:text "september"}]]]
    [c/pattern-grid {:pattern patterns/cells-pattern}]]])

(defn daily-page [{:as opts :keys [date]}]
  [c/page {:name (get-daily-page-name date)
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
        (->> (range 11 12)
          (mapv (fn [month]
                  [monthly-page {:date (t/new-date 2024 month 1)}])))

        (->> (range-days
               (t/new-date 2024 10 28)
               (t/new-date 2025 1 5))
          (mapv (fn [date]
                  [daily-page {:date date}])))))))
