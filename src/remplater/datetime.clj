(ns remplater.datetime
  (:require
    [tick.core :as t]))

(defn range-dates [from to & [step]]
  (let [step (or step (t/of-days 1))]
    (->> from
      (iterate #(t/>> % step))
      (take-while #(t/<= % to)))))
