(ns remplater.templates.remarkable-calendar
  (:require
    [clojure.string :as str]
    [remplater.components :as c]
    [remplater.fig-operations :as fo]
    [remplater.patterns :as patterns]
    [remplater.pdf :as pdf]
    [remplater.render :as render]
    [tick.core :as t]))

(def dt-formatter-year (t/formatter "yyyy"))
(def dt-formatter-short-month (t/formatter "M"))
(def dt-formatter-long-month (t/formatter "MM"))
(def dt-formatter-text-month (t/formatter "MMMM"))
(def dt-formatter-short-day (t/formatter "d"))
(def dt-formatter-long-day (t/formatter "dd"))
(def dt-formatter-week-day (t/formatter "EEEE"))

(def cells-pattern
  {:width 45
   :height 45
   :line (fn [{:as fig-opts :keys [col-index row-index]}]
           (if (= 1 col-index)
             [c/line {:color (pdf/make-color 0 0 0)
                      :width 4}]
             [c/line {:color (pdf/make-color 100 100 100)}]))
   :outline [c/line {:color (pdf/make-color 100 100 100)}]
   :row (fn [{:as fig :keys [row-index rows]}]
          (when (= 11 row-index)
            (let [this-and-next-rows (fo/join-two (get rows row-index) (get rows (inc row-index)))]
              [c/split (merge this-and-next-rows
                         {:direction :x :splits [(* 2 (:width cells-pattern))]})
               [c/text {:text "12"
                        :font-size 40
                        :valign :center
                        :halign :center
                        :text-offset 10
                        :children-offset 0}
                [c/margin {:margin -5}
                 [c/rect {:fill-color (pdf/make-color 255 255 255)}]]]])))})

(defn date->units [date]
  {:year (t/format dt-formatter-year date)
   :month (t/format dt-formatter-short-month date)
   :day (t/format dt-formatter-short-day date)})

(defn get-montly-page-name [date]
  (let [{:keys [year month]} (date->units date)]
    (str "monthly-page-" year "-" month)))

(defn get-daily-page-name [date]
  (let [{:keys [year month day]} (date->units date)]
    (str "daily-page-" year "-" month "-" day)))

(defn range-dates [from to & [step]]
  (let [step (or step (t/of-days 1))]
    (->> from
      (iterate #(t/>> % step))
      (take-while #(t/<= % to)))))

(defn get-monthly-days [{:keys [date to-date]}]
  (let [month-start (t/first-day-of-month date)
        calendar-start-day (t/previous-or-same month-start t/MONDAY)
        calendar-end-day (t/min to-date (t/>> calendar-start-day (t/of-days 34)))]
    (->> (range-dates calendar-start-day calendar-end-day)
      (mapv (fn [date]
              {:label (t/format dt-formatter-long-day date)
               :this-month? (= (t/month date) (t/month month-start))
               :weekend? (#{t/SATURDAY t/SUNDAY} (t/day-of-week date))
               :page-name (get-daily-page-name date)})))))

(defn monthly-page [{:as opts :keys [date to-date]}]
  (let [days (get-monthly-days opts)]
    [:page {:size pdf/remarkable-2-horizontal-page-size
            :name (get-montly-page-name date)}
     [c/margin {:margin 80
                :margin-top 50}
      [c/split {:direction :y :splits [200]}

       ;; header
       [c/margin {:margin-top 40
                  :margin-bottom 40}
        [c/split {:direction :x :splits [#(/ % 7)]}
         [c/div {}
          [c/border {:border-right true
                     :width 4}]
          [c/margin {:margin-right 30}
           [c/split {:direction :y :splits [50]}
            [c/text {:text (t/format dt-formatter-long-month date)
                     :font-size 70
                     :valign :center
                     :halign :right}]
            [c/text {:text (t/format dt-formatter-year date)
                     :font-size 50
                     :valign :center
                     :halign :right}]]]]
         [c/div {}
          [c/margin {:margin-left 40}
           [c/text {:text (t/format dt-formatter-text-month date)
                    :text-offset 30
                    :font-size 120
                    :valign :top
                    :halign :left}]]]]]

       ;; days grid
       [c/grid {:rows 5 :cols 7}
        (fn [fig-opts]
          (let [{:as day-info :keys [label page-name this-month? weekend?]} (get days (:index fig-opts))]
            (when day-info
              [[c/page-link {:target-page page-name}]
               [c/rect (if weekend?
                         {:fill? true
                          :fill-color (pdf/make-color 230 230 230)
                          :stroke? true
                          :line-width 4}
                         {:fill? false
                          :stroke? true
                          :line-width 4})]
               [c/margin {:margin-top 10
                          :margin-left 20}
                [c/text {:text label
                         :font-size 50
                         :fill-color (if this-month?
                                       (pdf/make-color 0 0 0)
                                       (pdf/make-color 160 160 160))}]]])))]]]]))

(defn daily-layout [{:keys [date]}]
  [c/aligned-pattern-wrapper {:pattern cells-pattern
                              :horizontal-align :center}
   [c/split {:direction :y :splits [150]}
    [c/split {:direction :x :splits [(* 4 (:width cells-pattern))]}
     [c/div {}
      [c/border {:border-right true
                 :width 4}]
      [c/margin {:margin-right 20}
       [c/text {:text (t/format dt-formatter-long-day date)
                :font-size 130
                :text-offset 30
                :halign :center
                :valign :center}]]]
     [c/margin {:margin-left 30}
      [c/split {:direction :y :splits [70]}
       [c/text {:text (-> (t/format dt-formatter-week-day date)
                        (str/upper-case))
                :font-size 70
                :valign :top
                :halign :left}]
       [c/text {:text (t/format dt-formatter-text-month date)
                :font-size 50
                :valign :top
                :halign :left}
        [c/page-link {:target-page (get-montly-page-name date)}]]]]]
    [c/pattern-grid {:pattern cells-pattern}]]])

(defn daily-page [{:as opts :keys [left-page-date to-date]}]
  (let [right-page-date (t/>> left-page-date (t/of-days 1))]
    [:page {:name (get-daily-page-name left-page-date)
            :aliases [(get-daily-page-name right-page-date)]
            :size pdf/remarkable-2-horizontal-page-size}
     [c/margin {:margin 80
                :margin-top 50}
      [c/split {:direction :x
                :splits [#(/ % 2)]}
       [c/margin {:margin-right 20}
        [daily-layout {:date left-page-date}]]
       [c/margin {:margin-left 20}
        (when (t/<= right-page-date to-date)
          [daily-layout {:date right-page-date}])]]]]))

(defn remarkable-calendar [{:keys [from-date to-date]}]
  (into [:document {:output "/tmp/remarkable_calendar.pdf"
                    :fonts {:default "fonts/GentiumPlus-6.200/GentiumPlus-Regular.ttf"}}]
    (concat
      (->> (range-dates from-date to-date (t/of-months 1))
        (mapv (fn [date]
                [monthly-page {:to-date to-date
                               :date date}])))

      (->> (range-dates from-date to-date (t/of-days 2))
        (mapv (fn [date]
                [daily-page {:left-page-date date
                             :to-date to-date}]))))))

(comment
  (render/render-document
    (remarkable-calendar
      {:from-date (t/new-date 2024 1 1)
       :to-date (t/new-date 2025 1 31)})))
