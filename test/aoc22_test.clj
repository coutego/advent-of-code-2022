(ns aoc22-test
  (:require [clojure.test :refer [deftest is]]
            [aoc22 :as c]))

(deftest d01p1
 (let [data [199 200 208 210 200 207 240 269 260 263]]
   (is (= 7 (c/num-increases data)))))
