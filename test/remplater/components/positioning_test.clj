(ns remplater.components.positioning-test
  (:require
    [clojure.test :refer [deftest is testing]]
    [remplater.components.positioning :as pos]))

(deftest attrs->sizes
  (is (= [100 100] (pos/attrs->sizes {:x1 0 :y1 0 :x2 100 :y2 100})))
  (is (= [100 100] (pos/attrs->sizes {:x1 -50 :y1 -50 :x2 50 :y2 50})))
  (is (= [100 100] (pos/attrs->sizes {:x1 50 :y1 50 :x2 -50 :y2 -50}))))

(deftest split
  (testing "split by x (numbers)"
    (is (= [{:x1 0 :y1 0 :x2 25 :y2 100}
            {:x1 25 :y1 0 :x2 50 :y2 100}
            {:x1 50 :y1 0 :x2 100 :y2 100}]
          (pos/split {:x1 0 :y1 0 :x2 100 :y2 100} :x [25 25]))))

  (testing "split by y (numbers)"
    (is (= [{:x1 0 :y1 75 :x2 100 :y2 100}
            {:x1 0 :y1 50 :x2 100 :y2 75}
            {:x1 0 :y1 0 :x2 100 :y2 50}]
          (pos/split {:x1 0 :y1 0 :x2 100 :y2 100} :y [25 25]))))

  (testing "split by x (numbers and function)"
    (is (= [{:x1 0 :y1 0 :x2 50 :y2 100}
            {:x1 50 :y1 0 :x2 75 :y2 100}
            {:x1 75 :y1 0 :x2 100 :y2 100}]
          (pos/split {:x1 0 :y1 0 :x2 100 :y2 100} :x [50 #(/ % 2)])))))

(deftest join
  (testing "vertical join"
    (is (= {:x1 0 :y1 0 :x2 100 :y2 100}
          (pos/join
            {:x1 0 :y1 0 :x2 100 :y2 50}
            {:x1 0 :y1 50 :x2 100 :y2 100})))
    (is (= {:x1 0 :y1 0 :x2 100 :y2 100}
          (pos/join
            {:x1 0 :y1 50 :x2 100 :y2 100}
            {:x1 0 :y1 0 :x2 100 :y2 50})))
    (is (= {:x1 0 :y1 0 :x2 100 :y2 100}
          (pos/join
            {:x1 0 :y1 75 :x2 100 :y2 100}
            {:x1 0 :y1 50 :x2 100 :y2 75}
            {:x1 0 :y1 0 :x2 100 :y2 50}))))

  (testing "horizontal join"
    (is (= {:x1 0 :y1 0 :x2 100 :y2 100}
          (pos/join
            {:x1 0 :y1 0 :x2 50 :y2 100}
            {:x1 50 :y1 0 :x2 100 :y2 100})))
    (is (= {:x1 0 :y1 0 :x2 100 :y2 100}
          (pos/join
            {:x1 50 :y1 0 :x2 100 :y2 100}
            {:x1 0 :y1 0 :x2 50 :y2 100})))
    (is (= {:x1 0 :y1 0 :x2 100 :y2 100}
          (pos/join
            {:x1 0 :y1 0 :x2 25 :y2 100}
            {:x1 25 :y1 0 :x2 50 :y2 100}
            {:x1 50 :y1 0 :x2 100 :y2 100}))))

  (testing "incorrect join"
    (is (nil? (pos/join
                {:x1 0 :y1 0 :x2 100 :y2 50}
                {:x1 0 :y1 200 :x2 100 :y2 400})))
    (is (nil? (pos/join
                {:x1 0 :y1 0 :x2 50 :y2 50}
                {:x1 100 :y1 100 :x2 200 :y2 20})))))

(deftest margin
  (is (= {:x1 25 :y1 25 :x2 75 :y2 75}
        (pos/margin {:x1 0 :y1 0 :x2 100 :y2 100} 25)))
  (is (= {:x1 25 :y1 0 :x2 75 :y2 100}
        (pos/margin {:x1 0 :y1 0 :x2 100 :y2 100} 0 25)))
  (is (= {:x1 25 :y1 0 :x2 100 :y2 100}
        (pos/margin {:x1 0 :y1 0 :x2 100 :y2 100} 25 0 0 0)))
  (is (= {:x1 25 :y1 0 :x2 100 :y2 100}
        (pos/margin
          {:x1 0 :y1 0 :x2 100 :y2 100}
          {:margin-left 25})))
  (is (= {:x1 25 :y1 25 :x2 75 :y2 75}
        (pos/margin
          {:x1 0 :y1 0 :x2 100 :y2 100}
          {:margin-left 25
           :margin-right 25
           :margin-top 25
           :margin-bottom 25}))))

(deftest grid
  (is (= [{:x1 0 :y1 25 :x2 25 :y2 50 :index 0}
          {:x1 25 :y1 25 :x2 50 :y2 50 :index 1}
          {:x1 0 :y1 0 :x2 25 :y2 25 :index 2}
          {:x1 25 :y1 0 :x2 50 :y2 25 :index 3}]
        (pos/grid {:x1 0 :y1 0 :x2 50 :y2 50 :rows 2 :cols 2})))

  (is (= [{:x1 0 :y1 0 :x2 20 :y2 60 :index 0}
          {:x1 20 :y1 0 :x2 40 :y2 60 :index 1}
          {:x1 40 :y1 0 :x2 60 :y2 60 :index 2}]
        (pos/grid {:x1 0 :y1 0 :x2 60 :y2 60 :rows 1 :cols 3})))

  (is (= [{:x1 0 :y1 0 :x2 50 :y2 50 :index 0}]
        (pos/grid {:x1 0 :y1 0 :x2 50 :y2 50 :rows 1 :cols 1}))))

(deftest rect->border-line
  (let [attrs {:x1 0 :y1 0 :x2 100 :y2 100}
        bottom {:x1 0 :y1 0 :x2 100 :y2 0}
        top {:x1 0 :y1 100 :x2 100 :y2 100}
        left {:x1 0 :y1 0 :x2 0 :y2 100}
        right {:x1 100 :y1 0 :x2 100 :y2 100}]
    (is (= bottom (pos/rect->border-line attrs :bottom)))
    (is (= top (pos/rect->border-line attrs :top)))
    (is (= left (pos/rect->border-line attrs :left)))
    (is (= right (pos/rect->border-line attrs :right)))
    (is (= {:top top :left left :right right :bottom bottom}
          (pos/rect->border-lines attrs)))))

(deftest rect->center
  (is (= {:x 50 :y 50}
        (pos/rect->center {:x1 0 :y1 0 :x2 100 :y2 100})))
  (is (= {:x 50 :y 0}
        (pos/rect->center {:x1 0 :y1 0 :x2 100 :y2 0}))))
