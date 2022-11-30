(ns aoc22
  (:require [clojure.string :as st]))

(defn read-input-day
  "Read the filename 's'.txt from resources, applying 'parse-line' function to any function, if indicated"
  [s & [parse-line]]
  (let [fname (str "resources/" s ".txt")
        ret   (st/split-lines (slurp fname))
        ret   (if parse-line (map parse-line ret) ret)]
    ret))

(defn num-increases [nums] (->> (map < nums (rest nums))
                                (filter true?)
                                count))

(defn d1p1 []
  (->> (read-input-day "d1p1" #(Integer/valueOf %))
       num-increases))

(defn run-f
  "Run solution for day d (1, 25) and part p (1, 2)"
  [d p]
  (let [f (find-var (symbol (str "aoc22/" (str "d" d "p" p))))]
    (when-not (nil? f) (str "Result for day " d ", part " p ": " (f)))))

(defn -main
  "Print the solutions for all solved puzzles so far"
  [& _]
  (->> (for [d (range 1 26), p (range 1 3)] (run-f d p))
       (filter (comp not nil?))
       (map println)
       doall))
