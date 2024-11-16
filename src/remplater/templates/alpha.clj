(ns remplater.templates.alpha
  (:require
    [clojure.string :as str]
    [remplater.components.builtin-components]
    [remplater.components.render :as render]
    [remplater.datetime :as dt]
    [remplater.pdf :as pdf]
    [tick.core :as t]))

#_(def timeline-pattern
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

#_(def cells-pattern
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

#_(defn page-layout [attrs & children]
    [:margin {:margin-left 104}
     [:split {:direction :y :splits [104]}
      [:div
       [:split {:direction :x :splits [1000]}
        [:div (:header attrs)]
        [:div (:header-end attrs)]]]
      (into [:div] children)]])

#_(defn header-button [attrs & children]
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

#_(defn header-buttons [attrs & children]
    (let [splits (repeat (count children) 100)]
      (into [:split {:direction :x :splits splits}] children)))

#_(defn daily-page [{:keys [date]}]
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

#_(defn monthly-page [{:keys [date]}]
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

#_(defn notes-index [{:keys [index-page]}]
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

#_(defn notes-page [{:keys [note-index]}]
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

#_(defn monthly-inbox-page [attrs]
    [:page {:name "montly-inbox"}
     [page-layout {:header [:text {:text "September Inbox"
                                   :font-size 80
                                   :valign :center
                                   :halign :left}]
                   :header-end [header-buttons
                                [header-button {:title "n"}]]}
      [:margin {:margin 40}
       [:pattern-grid {:pattern cells-pattern}]]]])

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
     [:split {:direction :x :splits [#(* % 3/4)]}
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
                 :halign :left}]]]]
      [:padding {:padding-top 190}
       [:text {:text "NOTES>"
               :font-size 35
               :halign :left}]]]

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
