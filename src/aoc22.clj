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
    (->> (for [day (range 1 26)
               part (range 1 3)]
           (run-f day part))
         (filter identity)
         (run! println))))

(defn map-apply [f xs]
  (map #(apply f %) xs))

(defn in? [xs el]
  (some #(= el %) xs))

;; Day 1
(defn has-no-nils [xs]
  (not (some nil? xs)))

(defn partition-by-nils [xs]
  (->> (partition-by nil? xs)
       (filter has-no-nils)))

(defn calories [nums]
  (->> (partition-by-nils nums)
       (map-apply +)))

(defn sum-top-n [n nums]
  (->> (sort > nums)
       (take n)
       (reduce +)))

(defn d1p1 []
  (->> (read-input-day "d1" parse-opt-int)
       calories
       (apply max)))

(defn d1p2 []
  (->> (read-input-day "d1" parse-opt-int)
       calories
       (sum-top-n 3)))

(deftest d01
  (let [data [1000 2000 3000 nil 4000 nil 5000 6000 nil 7000 8000 9000 nil 10000]]
    (is (= 24000 (apply max (calories data))))
    (is (= 71934 (d1p1)))
    (is (= 211447 (d1p2)))))

;; Day 2
;; We use (and abuse) the fact that Rock, Paper, Scissors is best
;; described and implemented in arithmetic modulo 3
(def plays ["A" "B" "C" "X" "Y" "Z"])

(defn play2n [p]
  (-> (.indexOf plays p)
      (mod 3)))

(defn result-play [p1 p2]
  (-> (- (play2n p2) (play2n p1))
      inc
      (mod 3)))

(defn score-play [p1 p2]
  (+ (inc (play2n p2))
     (* 3
        (result-play p1 p2))))

(defn score-strategy [st]
  (->> st
       (map-apply score-play)
       (reduce +)))

(defn parse-play [s]
  (st/split s #" "))

(defn parse-play-win-lose [s]
  (let [[p r] (parse-play s)]
    [p
     (-> r
         play2n
         dec
         (+ (play2n p))
         (mod 3)
         plays)]))

(defn d2p1 []
  (-> (read-input-day "d2" parse-play)
      score-strategy))

(defn d2p2 []
  (-> (read-input-day "d2" parse-play-win-lose)
      score-strategy))

(deftest d02
  (is (= 8 (score-play "A" "B")))
  (is (= 1 (score-strategy [["B" "A"]])))
  (is (= 15 (score-strategy [["A" "Y"] ["B" "X"] ["C" "Z"]])))
  (is (= 11767 (d2p1)))
  (is (= 13886 (d2p2))))

;; Day 3
(defn priority [it]
  (if (> (int it) 91)
    (- (int it) 96)
    (- (int it) 38))) ;; ascii codes

(defn take-drop-half [tk? xs]
  (let [n (/ (count xs) 2)]
    (if tk?
      (take n xs)
      (drop n xs))))

(defn half-n [n xs]
  (-> (= 0 (mod n 2))
      (take-drop-half xs)
      set))

(defn item-both-comp [rsack]
  (first (set/intersection (half-n 0 rsack) (half-n 1 rsack))))

(defn sum-priorities [rsacks]
  (->> rsacks
       (map item-both-comp)
       (map priority)
       (reduce +)))

(defn common-item [rsacks]
  (->> rsacks
       (map set)
       (apply set/intersection)
       first))

(defn sum-badges [groups]
  (->> groups
       (map common-item)
       (map priority)
       (reduce +)))

(defn d3p1 []
  (->> (read-input-day "d3")
       sum-priorities))

(defn d3p2 []
  (->> (read-input-day "d3")
       (partition 3)
       sum-badges))

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

;; Day 4
(defn fully-contained? [r1 r2]
  (and (>= (first r1) (first r2))
       (<= (second r1) (second r2))))

(defn fully-overlap? [[r1 r2]]
  (or (fully-contained? r1 r2)
      (fully-contained? r2 r1)))

(defn in-range? [[a b] n]
  (and (<= a n)
       (<= n b)))

(defn range-in-range? [r [a b]]
  (or (in-range? r a)
      (in-range? r b)))

(defn overlap? [[r1 r2]]
  (or (range-in-range? r1 r2)
      (range-in-range? r2 r1)))

(defn parse-d4 [line]
  (->> (st/split line #"[,-]")
       (map parse-int)
       (partition 2)))

(defn d4p1 []
  (->> (read-input-day "d4" parse-d4)
       (filter fully-overlap?)
       count))

(defn d4p2 []
  (->> (read-input-day "d4" parse-d4)
       (filter overlap?)
       count))

(deftest d04
  (is (fully-contained? [3 7] [2 8]))
  (is (fully-overlap? [[2 8] [3 7]] ))
  (is (= 2 (->> (read-input-day "d4-test" parse-d4) (filter fully-overlap?) count)))
  (is (= 644 (d4p1)))
  (is (= 4 (->> (read-input-day "d4-test" parse-d4) (filter overlap?) count)))
  (is (= 926 (d4p2))))

;; Day 5
(defn read-move [line]
  (->> (re-seq #"move (\d+) from (\d+) to (\d+)" line)
       (first)
       (drop 1)
       (map parse-int)))

(defn read-moves [lines]
  (->> lines
       (map read-move)))

(defn stack-char-reducer [[stacks idx] nchar]
  (if (= nchar \space)
    [stacks (inc idx)]
    [(update-in stacks
                [idx]
                (fn [stack] (conj stack nchar)))
     (inc idx)]))

(defn stack-line-reducer [stacks nline]
  (first
   (reduce stack-char-reducer [stacks 0] nline)))

(defn read-stacks [lines n]
  (->> lines
       (reduce stack-line-reducer (vec (for [i (range n)] [])))
       (map reverse)
       (mapv vec)))

(defn read-d5 [& [f]]
  (let [lines (read-input-day (or f "d5"))
        [rows cols] (->> lines
                         first
                         (re-seq #"(\d+) (\d+)")
                         first
                         (drop 1)
                         (map parse-int))
        lines (rest lines)
        stacks (read-stacks (take rows lines) cols)
        moves (read-moves (drop rows lines))]
    {:stacks stacks
     :moves moves}))

(defn move-element [stacks from to]
  (let [from (dec from)
        to (dec to)
        el (last (get-in stacks [from]))]
    (-> stacks
        (update-in [from] pop)
        (update-in [to] #(conj % el)))))

(defn procces-move [stacks [n from to]]
  (if (= n 0)
    stacks
    (recur (move-element stacks from to) [(dec n) from to])))

(defn process-moves [{:keys [stacks moves]}]
  (reduce procces-move stacks moves))

(defn crates-on-top [stacks]
  (reduce (fn [acc n] (str acc (last n))) "" stacks))

(defn process-move-in-bulk [stacks [n from to]]
  (if (= n 0)
    stacks
    (let [from (dec from)
          to (dec to)
          els (get stacks from)
          els (subvec els (- (count els) n))]
      (-> stacks
          (update-in [from] #(vec (drop-last n %)))
          (update-in [to] #(vec (concat % els)))))))

(defn process-moves-in-bulk [{:keys [stacks moves]}]
  (reduce process-move-in-bulk stacks moves))

(defn d5p1
  ([& [filename]]
   (->> (read-d5 (or filename "d5"))
        process-moves
        crates-on-top)))

(defn d5p2
  ([& [filename]]
   (->> (read-d5 (or filename "d5"))
        process-moves-in-bulk
        crates-on-top)))

(deftest d05
  (is (= "CMZ" (->> (d5p1 "d5-test"))))
  (is (= "JDTMRWCQJ" (d5p1)))
  (is (= "MCD" (->> (d5p2 "d5-test"))))
  (is (= "VHJDDCWRD" (d5p2))))
