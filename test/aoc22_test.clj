(ns aoc22-test
  (:require [clojure.test :refer [deftest is]]
            [aoc22 :as c]))

(deftest d01p1
 (let [data [1000 2000 3000 nil 4000 nil 5000 6000 nil 7000 8000 9000 nil 10000]]
   (is (= 24000 (apply max (c/calories data))))
   (is (= 71934 (c/d1p1)))
   (is (= 211447 (c/d1p2)))))
