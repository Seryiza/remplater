(ns remplater.templates.alpha
  (:require
    [clojure.string :as str]
    [remplater.components.builtin-components]
    [remplater.components.render :as render]
    [remplater.datetime :as dt]
    [remplater.pdf :as pdf]
    [tick.core :as t]))

(def timeline-pattern
  {:width 150
   :height 75
   :row (fn [{:keys [row-index rows start-hour]}]
          (let [hour? (even? row-index)
                hour (rem (+ start-hour (/ row-index 2)) 24)]
            [:split {:direction :x :splits [50 #(- % (/ (:width timeline-pattern) 2))]}
             [:div (when hour?
                     [:margin {:margin-right 10}
                      [:text {:text (str hour)
                              :font-size 25
                              :valign :center
                              :halign :right}]])]
             [:line {:color (if hour?
                              (pdf/make-color 100 100 100)
                              (pdf/make-color 0 0 0))
                     :line-type :horizontal-middle
                     :dash (when-not hour?
                             {:pattern [1 3]
                              :phase 0})}]]))
   :col (fn [{:keys [col-index cols]}]
          [:margin {:margin-top (/ (:height timeline-pattern) 2)
                    :margin-bottom (/ (:height timeline-pattern) 2)}
           [:line {:line-type :vertical-middle}]])})

(def cells-pattern
  {:width 45
   :height 45
   :line (fn [{:keys [col-index row-index strong-line-rows]}]
           (let [strong-line? (contains? strong-line-rows row-index)]
             [:line {:color (if strong-line?
                                (pdf/make-color 0 0 0)
                                (pdf/make-color 100 100 100))
                     :width (if strong-line? 4 1)}]))
   :outline [:line {:color (pdf/make-color 100 100 100)}]})

(defn get-month-page-name [date]
  (str "month-page-" (t/format dt/fmt-yyyy date) "-" (t/format dt/fmt-mm date)))

(defn get-month-inbox-page-name [date]
  (str "month-inbox-page-" (t/format dt/fmt-yyyy date) "-" (t/format dt/fmt-mm date)))

(defn get-day-page-name [date]
  (str "day-page-" (t/format dt/fmt-yyyy date) "-" (t/format dt/fmt-mm date) "-" (t/format dt/fmt-dd date)))

(defn get-notes-index-page-name [page-index]
  (str "notes-index-page-" page-index))

(defn get-notes-index-alias-by-note [note-index]
  (str "notes-index-page-by-note-" note-index))

(defn get-notes-page-name [note-index]
  (str "notes-page-" note-index))

(defn get-monthly-days [{:keys [date]}]
  (let [month-start (t/first-day-of-month date)
        calendar-start-day (t/previous-or-same month-start t/MONDAY)
        calendar-end-day (t/>> calendar-start-day (t/of-days 41))]
    (->> (dt/range-dates calendar-start-day calendar-end-day)
      (mapv (fn [date]
              {:label (t/format dt/fmt-dd date)
               :page-name (get-day-page-name date)
               :this-month? (= (t/month date) (t/month month-start))
               :weekend? (dt/weekend? date)})))))

(defn page-layout [attrs & children]
  [:margin {:margin-left 104}
   [:split {:direction :y :splits [104]}
    [:div
     [:split {:direction :x :splits [1000]}
      [:div (:header attrs)]
      [:div (:header-end attrs)]]]
    (into [:div] children)]])

(defn header-button [attrs & children]
  [:div
   (when-let [page-link (:page-link attrs)]
     [:page-link {:target-page page-link}])

   [:margin {:margin 30}
    [:circle {:fill? false
              :stroke? true
              :line-width 4
              :radius 20}
     [:text {:text (:title attrs)
             :font-size 30
             :valign :center
             :halign :center}]]]])

(defn header-buttons [attrs & children]
  (let [splits (repeat (count children) 100)]
    (into [:split {:direction :x :splits splits}] children)))

(defn daily-page [{:keys [date]}]
  [:page {:name (get-day-page-name date)}
   [page-layout {:header [:split {:direction :x :splits [500]}
                          [:text {:text (t/format dt/fmt-mmmm date)
                                  :font-size 80
                                  :valign :center
                                  :halign :left}
                           [:page-link {:target-page (get-month-page-name date)}]]
                          [:text {:text (t/format dt/fmt-dd date)
                                  :font-size 80
                                  :valign :center
                                  :halign :left}]]
                 :header-end [header-buttons
                              [header-button {:title "n"
                                              :page-link (get-notes-index-page-name 0)}]]}
    [:split {:direction :x :splits [#(/ % 2)]}
     [:margin {:margin-top 40
               :margin-bottom 40}
      [:pattern-grid {:pattern cells-pattern
                      :strong-line-rows #{}}]]
     [:pattern-grid {:pattern timeline-pattern
                     :start-hour 10}]]]])

(defn monthly-page [{:keys [date]}]
  (let [monthly-dates (get-monthly-days {:date date})]
    [:page {:name (get-month-page-name date)}
     [page-layout {:header [:text {:text (str
                                           (t/format dt/fmt-mmmm date)
                                           " "
                                           (t/format dt/fmt-yyyy date))
                                   :font-size 80
                                   :valign :center
                                   :halign :left}]
                   :header-end [header-buttons
                                [header-button {:title "i"}]
                                [header-button {:title "n"}]]}
      [:split {:direction :y :splits [1000]}
       [:margin {:margin 40}
        [:grid {:rows 6 :cols 7}
         (fn [{:keys [index]}]
           (let [{:keys [label page-name this-month?]} (get monthly-dates index)]
             (when this-month?
               [:div
                [:page-link {:target-page page-name}]
                [:rect {:fill? false
                        :stroke? true
                        :line-width 3}]
                [:margin {:margin 10}
                 [:text {:text label
                         :font-size 40
                         :valign :top
                         :halign :left}]]])))]]
       [:margin {:margin 40}
        [:pattern-grid {:pattern cells-pattern}]]]]]))

(defn notes-index [{:keys [index-page]}]
  (let [start-note-index (* 50 index-page)
        max-note-index (+ 50 start-note-index)]
    [:page {:name (get-notes-index-page-name index-page)
            :aliases (->> (range start-note-index max-note-index)
                       (mapv #(get-notes-index-alias-by-note %)))}
     [page-layout {:header [:text {:text "Notes"
                                   :font-size 80
                                   :valign :center
                                   :halign :left}]}
      [:margin {:margin 20}
       [:grid {:rows 25 :cols 2}
        (fn [{:keys [index]}]
          [:div
           [:page-link {:target-page (get-notes-page-name (+ index start-note-index))}]
           [:split {:direction :x :splits [60 60]}
            [:rect {:fill? false
                    :stroke? true}
             [:text {:text (str (+ index start-note-index 1))
                     :font-size 30
                     :halign :center
                     :valign :center}]]
            [:rect {:fill? false
                    :stroke? true}]
            [:margin {:margin-right 50}
             [:rect {:fill? false
                     :stroke? true}]]]])]]]]))

(defn notes-page [{:keys [note-index]}]
  [:page {:name (get-notes-page-name note-index)}
   [page-layout {:header [:div
                          [:page-link {:target-page (get-notes-index-alias-by-note note-index)}]
                          [:text {:text (str "Note " (inc note-index))
                                  :font-size 80
                                  :valign :center
                                  :halign :left}]]
                 :header-end [header-buttons
                              [header-button {:title "n"}]]}
    [:margin {:margin 40}
     [:pattern-grid {:pattern cells-pattern}]]]])

(defn monthly-inbox-page [attrs]
  [:page {:name "montly-inbox"}
   [page-layout {:header [:text {:text "September Inbox"
                                 :font-size 80
                                 :valign :center
                                 :halign :left}]
                 :header-end [header-buttons
                              [header-button {:title "n"}]]}
    [:margin {:margin 40}
     [:pattern-grid {:pattern cells-pattern}]]]])

(defn document [{:keys [from-date to-date]}]
  (into [:document {:output "/tmp/alpha.pdf"
                    :page-size pdf/remarkable-2-page-size
                    :fonts {:default "fonts/Alice-Regular.ttf"}}]
    (concat
      (->> (dt/range-dates from-date to-date (t/of-months 1))
        (mapv (fn [date]
                [monthly-page {:date date}])))
      (->> (range 2)
        (mapv (fn [index]
                [notes-index {:index-page index}])))
      (->> (range (* 2 50))
        (mapv (fn [note-index]
                [notes-page {:note-index note-index}])))
      (->> (dt/range-dates from-date to-date (t/of-days 1))
        (mapv (fn [date]
                [daily-page {:date date}]))))))

(comment
  (render/render-document
    (document {:from-date (t/new-date 2024 11 1)
               :to-date (t/new-date 2024 11 30)})))
