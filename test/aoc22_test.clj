(ns aoc22.core-test
  (:require [clojure.test :refer [deftest testing is]]
            [aoc22.core :as c]))

(deftest d01p2
 (let [data [199 200 208 210 200 207 240 269 260 263]]
   (is (= 7 (c/num-increases data)))))
