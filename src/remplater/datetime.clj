(ns remplater.datetime
  (:require
    [tick.core :as t]))

(def fmt-yyyy (t/formatter "yyyy"))
(def fmt-m (t/formatter "M"))
(def fmt-mm (t/formatter "MM"))
(def fmt-mmmm (t/formatter "MMMM"))
(def fmt-d (t/formatter "d"))
(def fmt-dd (t/formatter "dd"))
(def fmt-day-of-week (t/formatter "EEEE"))

(defn range-dates [from to & [step]]
  (let [step (or step (t/of-days 1))]
    (->> from
      (iterate #(t/>> % step))
      (take-while #(t/<= % to)))))

(defn date->units [date]
  {:year (t/format fmt-yyyy date)
   :month (t/format fmt-m date)
   :day (t/format fmt-d date)})

(defn weekend? [date]
  (#{t/SATURDAY t/SUNDAY} (t/day-of-week date)))
