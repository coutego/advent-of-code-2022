(ns aoc22
  (:require [clojure.string :as st]
            [clojure.test :refer [deftest is]]
            [clojure.set :as set]))

;;
;; Utility code
;;

(def prefix "aoc22")

(defn read-input-day
  "Read the filename 's'.txt from resources, applying 'parse-line-fn' to all lines, if indicated"
  [s & [parse-line-fn]]
  (let [fname (str "resources/" prefix "/" s ".txt")
        ret   (st/split-lines (slurp fname))
        ret   (if parse-line-fn (map parse-line-fn ret) ret)]
    ret))

(defn parse-int [s] (Integer/valueOf s))

(defn parse-opt-int [s]
  (if (or (nil? s) (= (st/trim s) ""))
    nil
    (parse-int s)))

(defn -main
  "Print the solutions for all solved puzzles so far"
  [& _]
  (let [run-f (fn [day part]
                (let [f (find-var (symbol (str prefix "/" (str "d" day "p" part))))]
                  (when-not (nil? f)
                    (str "Result for day " day ", part " part ": " (f)))))]
    (->> (for [day (range 1 26), part (range 1 3)] (run-f day part))
         (filter identity)
         (run! println))))

(defn sum [nums] (reduce + nums))
(defn map-apply [f xs] (map #(apply f %) xs))

;; Day 1
(defn has-no-nils [xs] (not (some nil? xs)))
(defn partition-by-nils [xs] (->> (partition-by nil? xs) (filter has-no-nils)))
(defn calories [nums] (->> (partition-by-nils nums) (map-apply +)))
(defn sum-top-n [n nums] (->> (sort > nums), (take n), sum))

(defn d1p1 [] (->> (read-input-day "d1" parse-opt-int), calories, (apply max)))
(defn d1p2 [] (->> (read-input-day "d1" parse-opt-int), calories, (sum-top-n 3)))

(deftest d01
  (let [data [1000 2000 3000 nil 4000 nil 5000 6000 nil 7000 8000 9000 nil 10000]]
    (is (= 24000 (apply max (calories data))))
    (is (= 71934 (d1p1)))
    (is (= 211447 (d1p2)))))

;; Day 2
;; We use (and abuse) the fact that Rock, Paper, Scissors is best
;; described and implemented in arithmetic modulo 3
(def plays ["A" "B" "C" "X" "Y" "Z"])
(defn play2n [p] (-> (.indexOf plays p), (mod 3)))
(defn result-play [p1 p2] (-> (- (play2n p2) (play2n p1)), inc, (mod 3)))
(defn score-play [p1 p2] (+ (inc (play2n p2)) (* 3 (result-play p1 p2))))
(defn score-strategy [st] (->> st, (map-apply score-play), sum))
(defn parse-play [s] (st/split s #" "))

(defn parse-play-win-lose [s]
  (let [[p r] (parse-play s)]
    [p (-> r, play2n, dec, (+ (play2n p)), (mod 3), plays)]))

(defn d2p1 [] (-> (read-input-day "d2" parse-play), score-strategy))
(defn d2p2 [] (-> (read-input-day "d2" parse-play-win-lose), score-strategy))

(deftest d02
  (is (= 8 (score-play "A" "B")))
  (is (= 1 (score-strategy [["B", "A"]])))
  (is (= 15 (score-strategy [["A" "Y"] ["B" "X"] ["C" "Z"]])))
  (is (= 11767 (d2p1)))
  (is (= 13886 (d2p2))))

;; Day 3
(defn priority [it] (if (> (int it) 91) (- (int it) 96) (- (int it) 38))) ;; ascii codes
(defn take-drop-half [tk? xs] (let [n (/ (count xs) 2)] (if tk? (take n xs) (drop n xs))))
(defn half-n [n xs] (-> (= 0 (mod n 2)), (take-drop-half xs), set))
(defn item-both-comp [rsack] (first (set/intersection (half-n 0 rsack) (half-n 1 rsack))))
(defn sum-priorities [rsacks] (->> rsacks, (map item-both-comp), (map priority), sum))
(defn common-item [rsacks] (->> rsacks, (map set), (apply set/intersection), first))
(defn sum-badges [groups] (->> groups, (map common-item), (map priority), sum))

(defn d3p1 [] (->> (read-input-day "d3"), sum-priorities))
(defn d3p2 [] (->> (read-input-day "d3"), (partition 3), sum-badges))

(deftest d03
  (is (= #{\a \b} (set (half-n 0 "abaZ"))))
  (is (= [\a \Z] (vec (take-drop-half false "abaZ"))))
  (is (= \a (item-both-comp "abaZ")))
  (is (= 28 (sum-priorities ["abac" "aAbA"])))
  (let [data ["vJrwpWtwJgWrhcsFMMfFFhFp"
              "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
              "PmmdzqPrVvPwwTWBwg"
              "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
              "ttgJtRGJQctTZtZT"
              "CrZsJsPPZsGzwwsLwLmpwMDw"]]
    (is (= 157 (sum-priorities data)))
    (is (= 70 (sum-badges (partition 3 data)))))
  (is (= 7845 (d3p1)))
  (is (= 2790 (d3p2))))
