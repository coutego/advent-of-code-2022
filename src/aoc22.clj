(ns aoc22
  (:require [clojure.string :as st]))

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

(defn -main
  "Print the solutions for all solved puzzles so far"
  [& _]
  (let [run-f (fn [day part]
                (let [f (find-var (symbol (str prefix "/" (str "d" day "p" part))))]
                  (when-not (nil? f)
                    (str "Result for day " day ", part " part ": " (f)))))]
    (->> (for [day (range 1 26), part (range 1 3)] (run-f day part))
         (filter (comp not nil?))
         (map println)
         doall)))

;;
;; Day 1
;;
(defn num-increases [nums] (->> (map < nums (rest nums))
                                (filter identity)
                                count))

(defn three-window-sum [num]
  (map #(+ %1 %2 %3) num (rest num) (rest (rest num))))

(defn d1p1 []
  (->> (read-input-day "d1p1" parse-int)
       num-increases))

(defn d1p2 []
  (->> (read-input-day "d1p1" parse-int)
       three-window-sum
       num-increases))
