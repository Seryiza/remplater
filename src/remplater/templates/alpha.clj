(ns remplater.templates.alpha
  (:require
    [clojure.string :as str]
    [remplater.components.builtin-components]
    [remplater.components.render :as render]
    [remplater.datetime :as dt]
    [remplater.pdf :as pdf]
    [tick.core :as t]))

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

(def line-pattern-height 51)

(defn light-line [{:as attrs :keys [col-index]} & children]
  [:line {:dash {:pattern [1]
                 :phase 1}}])

(def line-pattern
  {:height line-pattern-height
   :row-count 30
   :line light-line
   :outline (fn [{:as attrs :keys [side]}]
              (when (= :bottom side)
                [light-line]))})

(def timegrid-pattern
  (merge line-pattern
    {:width "25%"}))

(def timeline-pattern
  (merge line-pattern
    {:line (fn [{:keys [row-index rows]}]
             (let [rows-count (count rows)
                   hour? (and (odd? row-index)
                           (<= 1 row-index)
                           (<= row-index (- rows-count 2)))
                   hour (+ 9 (/ (inc row-index) 2))]
               [:div
                [light-line {}]
                (when hour?
                  [:padding {:padding-right 70
                             :padding-bottom 0}
                   [:text {:text (str hour)
                           :font-size 25
                           :valign :center
                           :halign :right}
                    [:rect {:fill {:color (pdf/make-color 255 255 255)}}]]])]))}))

(defn double-vertical-line [attrs]
  [:div
   [:border {:border-right {:width 1}}]
   [:padding {:padding-right 10}
    [:border {:border-right {:width 1}}]]])

(defn single-horizontal-line [attrs]
  [:border {:border-bottom {:width 1}}])

(defn page-layout [attrs & children]
  (let [split-y 250]
    [:div
     [:split {:direction :x :splits [300]}
      [:split {:direction :y :splits [split-y]}
       [:div
        [double-vertical-line {}]
        [single-horizontal-line {}]
        (:top-left attrs)]
       [:div
        [double-vertical-line {}]
        (:bottom-left attrs)]]
      [:split {:direction :y :splits [split-y]}
       [:div
        [single-horizontal-line {}]
        (:top-right attrs)]
       [:div
        (:bottom-right attrs)]]]]))

(defn day-page [{:as attrs :keys [date]}]
  [:page {:name "day-page"}
   [page-layout
    {:top-left
     [:padding {:padding-top 90
                :padding-left 130}
      [:split {:direction :y :splits [100 40]}
       [:text {:text (t/format dt/fmt-dd date)
               :font-size 80
               :halign :center}]
       [:text {:text (str
                       (str/upper-case
                         (t/format dt/fmt-mmm date))
                       ">")
               :font-size 35
               :halign :center}]]]

     :top-right
     [:split {:direction :x :splits [#(/ % 2)]}
      [:split {:direction :x :splits [#(/ % 2)]}
       [:padding {:padding-top 90
                  :padding-left 30}
        [:split {:direction :y :splits [100]}
         [:text {:text (str/upper-case
                         (t/format dt/fmt-day-of-week date))
                 :font-size 80
                 :halign :left}]
         [:padding {:padding-left 5}
          [:text {:text (str "WEEK " (t/format dt/fmt-w date) ">")
                  :font-size 35
                  :halign :left}]]]]]
      [:split {:direction :x :splits [#(/ % 2)]}
       [:padding {:padding-top 190}
        [:text {:text "INBOX>"
                :font-size 35
                :halign :left}]]
       [:padding {:padding-top 190}
        [:text {:text "NOTES>"
                :font-size 35
                :halign :left}]]]]

     :bottom-left
     [:div
      [:pattern-grid {:pattern timeline-pattern
                      :vertical-align :top}]]

     :bottom-right
     [:div
      [:pattern-grid {:pattern timegrid-pattern
                      :vertical-align :top}]]}]])

(defn document [{:keys [from-date to-date]}]
  (into [:document {:output "/tmp/alpha.pdf"
                    :page-size pdf/remarkable-2-page-size
                    :fonts {:default "fonts/Iosevka-Regular.ttf"
                            :bold "fonts/Iosevka-Bold.ttf"}}]
    (concat
      [[day-page {:date (t/new-date 2024 11 17)}]]
      #_#_#_#_(->> (dt/range-dates from-date to-date (t/of-months 1))
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
