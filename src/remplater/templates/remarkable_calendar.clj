(ns remplater.templates.remarkable-calendar
  (:require
    [remplater.components :as c]
    [remplater.fig-operations :as fo]
    [remplater.pdf :as pdf]
    [remplater.render :as render]))

(defn month-page [])

(defn remarkable-calendar [])

(render/render-document
  [c/document {:output "/tmp/remarkable_calendar.pdf"}
   [c/page {:size pdf/remarkable-2-horizontal-page-size}
    [c/margin {:margin 50}
     [c/split {:direction :y :splits [200]}
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

      [c/grid {:rows 5 :cols 7}
       [(fn [fig-opts]
          [[c/rect {:fill? false
                    :stroke? true
                    :line-width 4}]
           [c/margin {:margin-top 10
                      :margin-left 20}
            [c/text {:text (str (:index fig-opts))
                     :font-size 40}]]])]]]]]])
