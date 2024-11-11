(ns remplater.components.positioning-test
  (:require
    [clojure.test :refer [deftest is testing]]
    [remplater.components.positioning :as pos]))

(deftest attrs->sizes
  (is (= [100 100] (pos/attrs->sizes {:x1 0 :y1 0 :x2 100 :y2 100})))
  (is (= [100 100] (pos/attrs->sizes {:x1 -50 :y1 -50 :x2 50 :y2 50})))
  (is (= [100 100] (pos/attrs->sizes {:x1 50 :y1 50 :x2 -50 :y2 -50}))))

(deftest split-one
  (testing "split by x (number)"
    (is (= [{:x1 0 :y1 0 :x2 50 :y2 100}
            {:x1 50 :y1 0 :x2 100 :y2 100}]
          (pos/split-one {:x1 0 :y1 0 :x2 100 :y2 100} :x 50))))

  (testing "split by y (number)"
    (is (= [{:x1 0 :y1 50 :x2 100 :y2 100}
            {:x1 0 :y1 0 :x2 100 :y2 50}]
          (pos/split-one {:x1 0 :y1 0 :x2 100 :y2 100} :y 50))))

  (testing "split by x (function)"
    (is (= [{:x1 0 :y1 0 :x2 50 :y2 100}
            {:x1 50 :y1 0 :x2 100 :y2 100}]
          (pos/split-one {:x1 0 :y1 0 :x2 100 :y2 100} :x #(/ % 2))))))

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

(deftest join-two
  (testing "vertical join"
    (is (= {:x1 0 :y1 0 :x2 100 :y2 100}
          (pos/join-two
            {:x1 0 :y1 0 :x2 100 :y2 50}
            {:x1 0 :y1 50 :x2 100 :y2 100})))
    (is (= {:x1 0 :y1 0 :x2 100 :y2 100}
          (pos/join-two
            {:x1 0 :y1 50 :x2 100 :y2 100}
            {:x1 0 :y1 0 :x2 100 :y2 50}))))

  (testing "incorrect join"
    (is (nil? (pos/join-two
                {:x1 0 :y1 0 :x2 100 :y2 50}
                {:x1 0 :y1 200 :x2 100 :y2 400})))
    (is (nil? (pos/join-two
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
