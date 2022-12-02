(ns aoc22-test
  (:require [clojure.test :refer [deftest is]]
            [aoc22 :as c]))

(deftest d01
  (let [data [1000 2000 3000 nil 4000 nil 5000 6000 nil 7000 8000 9000 nil 10000]]
    (is (= 24000 (apply max (c/calories data))))
    (is (= 71934 (c/d1p1)))
    (is (= 211447 (c/d1p2)))))

(deftest d02
  (let [data [["A" "Y"] ["B" "X"] ["C" "Z"]]]
    (is (= 15 (c/score-strategy data)))
    (is (= 8 (c/score-strategy [["A", "B"]])))
    (is (= 1 (c/score-strategy [["B", "A"]])))
    (is (= 6 (c/score-strategy [["C", "C"]])))
    (is (= 11767 (c/d2p1)))
    (is (= 13886 (c/d2p2)))))
