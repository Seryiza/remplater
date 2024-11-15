(ns remplater.templates.remarkable-calendar
  (:require
    [clojure.string :as str]
    [remplater.components.builtin-components]
    [remplater.components.render :as render]
    [remplater.datetime :as dt]
    [remplater.pdf :as pdf]
    [tick.core :as t]))

(def cells-pattern
  {:width 45
   :height 45
   :line (fn [{:keys [col-index row-index]}]
           (if (= 1 col-index)
             [:line {:color (pdf/make-color 0 0 0)
                     :width 4}]
             [:line {:color (pdf/make-color 100 100 100)}]))
   :outline [:line {:color (pdf/make-color 100 100 100)}]
   :row (fn [{:as fig :keys [timeline-labels row-index rows]}]
          (when-let [label (get timeline-labels row-index)]
            [:join {:joins [(get rows row-index)
                            (get rows (inc row-index))]}
             [:split {:direction :x :splits [(* 2 (:width cells-pattern))]}
              [:text {:text label
                      :font-size 40
                      :valign :center
                      :halign :center
                      :text-offset 10
                      :children-offset 0}
               [:margin {:margin -5}
                [:rect {:fill-color (pdf/make-color 255 255 255)}]]]]]))})

(defn get-montly-page-name [date]
  (let [{:keys [year month]} (dt/date->units date)]
    (str "monthly-page-" year "-" month)))

(defn get-daily-page-name [date]
  (let [{:keys [year month day]} (dt/date->units date)]
    (str "daily-page-" year "-" month "-" day)))

(defn get-monthly-days [{:keys [date from-date to-date]}]
  (let [month-start (t/first-day-of-month date)
        calendar-start-day (t/previous-or-same month-start t/MONDAY)
        calendar-end-day (t/>> calendar-start-day (t/of-days 34))]
    (->> (dt/range-dates calendar-start-day calendar-end-day)
      (mapv (fn [date]
              {:label (t/format dt/fmt-dd date)
               :page-name (get-daily-page-name date)
               :this-month? (= (t/month date) (t/month month-start))
               :weekend? (dt/weekend? date)
               :in-date-range? (t/<= from-date date to-date)})))))

(defn monthly-page [{:as opts :keys [date to-date]}]
  (let [days (get-monthly-days opts)]
    [:page {:name (get-montly-page-name date)}
     [:margin {:margin 80
               :margin-top 50}
      [:split {:direction :y :splits [200]}

       ;; header
       [:margin {:margin-top 40
                 :margin-bottom 40}
        [:split {:direction :x :splits [#(/ % 7)]}
         [:div {}
          [:border {:border-right {:width 4}}]
          [:margin {:margin-right 30}
           [:split {:direction :y :splits [50]}
            [:text {:text (t/format dt/fmt-mm date)
                    :font-size 70
                    :valign :center
                    :halign :right}]
            [:text {:text (t/format dt/fmt-yyyy date)
                    :font-size 50
                    :valign :center
                    :halign :right}]]]]
         [:div {}
          [:margin {:margin-left 40}
           [:text {:text (t/format dt/fmt-mmmm date)
                   :text-offset 30
                   :font-size 120
                   :valign :top
                   :halign :left}]]]]]

       ;; days grid
       [:grid {:rows 5 :cols 7}
        (fn [attrs]
          (let [{:keys [label page-name this-month? weekend? in-date-range?]}
                (get days (:index attrs))]
            (when in-date-range?
              [[:page-link {:target-page page-name}]
               [:rect (if weekend?
                        {:fill? true
                         :fill-color (pdf/make-color 230 230 230)
                         :stroke? true
                         :line-width 4}
                        {:fill? false
                         :stroke? true
                         :line-width 4})]
               [:margin {:margin-top 10
                         :margin-left 20}
                [:text {:text label
                        :font-size 50
                        :fill-color (if this-month?
                                      (pdf/make-color 0 0 0)
                                      (pdf/make-color 160 160 160))}]]])))]]]]))

(defn daily-layout [{:keys [date timeline-labels]}]
  [:aligned-pattern-wrapper {:pattern cells-pattern
                             :horizontal-align :center}
   [:split {:direction :y :splits [150]}
    [:split {:direction :x :splits [(* 4 (:width cells-pattern))]}
     [:div {}
      [:border {:border-right {:width 4}}]
      [:margin {:margin-right 20}
       [:text {:text (t/format dt/fmt-dd date)
               :font-size 130
               :text-offset 30
               :halign :center
               :valign :center}]]]
     [:margin {:margin-left 30}
      [:split {:direction :y :splits [70]}
       [:text {:text (-> (t/format dt/fmt-day-of-week date)
                       (str/upper-case))
               :font-size 70
               :valign :top
               :halign :left}]
       [:text {:text (t/format dt/fmt-mmmm date)
               :font-size 50
               :valign :top
               :halign :left}
        [:page-link {:target-page (get-montly-page-name date)}]]]]]
    [:pattern-grid {:pattern cells-pattern
                    :timeline-labels timeline-labels}]]])

(defn daily-page [{:as opts :keys [left-page-date to-date timeline-labels]}]
  (let [right-page-date (t/>> left-page-date (t/of-days 1))]
    [:page {:name (get-daily-page-name left-page-date)
            :aliases [(get-daily-page-name right-page-date)]}
     [:margin {:margin 80
               :margin-top 50}
      [:split {:direction :x
               :splits [#(/ % 2)]}
       [:margin {:margin-right 20}
        [daily-layout {:date left-page-date
                       :timeline-labels timeline-labels}]]
       [:margin {:margin-left 20}
        (when (t/<= right-page-date to-date)
          [daily-layout {:date right-page-date
                         :timeline-labels timeline-labels}])]]]]))

(defn document [{:keys [from-date to-date timeline-labels]}]
  (into [:document {:output "/tmp/remarkable_calendar.pdf"
                    :page-size pdf/remarkable-2-horizontal-page-size
                    :fonts {:default "fonts/GentiumPlus-6.200/GentiumPlus-Regular.ttf"}}]
    (concat
      (->> (dt/range-dates from-date to-date (t/of-months 1))
        (mapv (fn [date]
                [monthly-page {:from-date from-date
                               :to-date to-date
                               :date date}])))

      (->> (dt/range-dates from-date to-date (t/of-days 2))
        (mapv (fn [date]
                [daily-page {:left-page-date date
                             :to-date to-date
                             :timeline-labels timeline-labels}]))))))

(comment
  (render/render-document
    (document
      {:from-date (t/new-date 2024 1 1)
       :to-date (t/new-date 2025 1 31)
       :timeline-labels {11 "12"}})))
