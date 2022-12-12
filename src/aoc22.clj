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

(defn parse-int [s] (Integer/valueOf (st/trim s)))

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
(defn move-element [stacks from to]
  (let [stacks (mapv vec stacks)
        el (last (get-in stacks [from]))]
    (-> stacks
        (update-in [from] pop)
        (update-in [to] #(conj % el)))))

(defn process-move [stacks [n from to]]
  (if (= n 0)
    stacks
    (recur (move-element stacks (dec from) (dec to)) [(dec n) from to])))

(defn process-moves [stacks moves]
  (reduce process-move stacks moves))

(defn crates-on-top [stacks]
  (reduce (fn [acc n] (str acc (last n))) "" stacks))

(defn process-move-in-bulk [stacks [n from to]]
  (if (= n 0)
    stacks
    (let [from (dec from)
          to (dec to)
          stacks (mapv vec stacks)
          els (get stacks from)
          els (subvec els (- (count els) n))]
      (-> stacks
          (update-in [from] #(vec (drop-last n %)))
          (update-in [to] #(vec (concat % els)))))))

(defn process-moves-in-bulk [stacks moves]
  (reduce process-move-in-bulk stacks moves))

(defn transpose
  "Transpose a 'matrix' `mat`.
  `mat'`is any sequence of sequences representing the matrix rows, with the
  particularity that they don't need to extend to the number of columns.
  Any missing element at the end is interpreted a being nil."
  [mat]
  (->> mat
       (map (fn [row] (concat row (repeat nil)))) ;; pad with (infinite) nils
       (apply map vector)                         ;; transpose (map with n colls)
       (take-while #(some identity %))))          ;; finish when all elements are nil

(defn read-move [line]
  (->> (re-seq #"move (\d+) from (\d+) to (\d+)" line)
       first
       (drop 1)
       (map parse-int)))

(defn read-moves [lines]
  (->> lines
       (map read-move)))

(defn take-stack-cols
  "Select just the elements on the stacks, discarding brackets and whitespace"
  [s]
  (->> (for [i (range)] (get s (+ 1 (* 4 i))))
       (take-while identity)))

(defn read-stacks-moves [raw-stacks _ raw-moves]
  [(->> raw-stacks
        (map take-stack-cols)
        transpose
        (map reverse)
        (map #(take-while (fn [c] (and c (not= c \space))) %)))
   (read-moves raw-moves)])

(defn d5 [f & [filename]]
  (let [[stacks moves]
        (->> (read-input-day (or filename "d5"))
             (partition-by #(= "" %))
             (apply read-stacks-moves))]
    (->> (f stacks moves)
         crates-on-top)))

(defn d5p1 [& [filename]]
  (d5 process-moves filename))

(defn d5p2 [& [filename]]
  (d5 process-moves-in-bulk filename))

(deftest d05
  (is (= "CMZ" (->> (d5p1 "d5-test"))))
  (is (= "JDTMRWCQJ" (d5p1)))
  (is (= "MCD" (->> (d5p2 "d5-test"))))
  (is (= "VHJDDCWRD" (d5p2))))

;; Day 6
(defn different-els? [xs]
  (= (count xs) (count (set xs))))

(defn find-n-different [n s]
  (->> s
       (partition n 1)
       (filter different-els?)
       first
       (apply str)
       (st/index-of s)
       (+ n)))

(defn d6p1 []
  (->> (read-input-day "d6")
       first
       (find-n-different 4)))

(defn d6p2 []
  (->> (read-input-day "d6")
       first
       (find-n-different 14)))

(deftest d06
  (is (= 1760 (d6p1)))
  (is (= 2974 (d6p2))))

;; Day 7
;; Input is basically a tree of s-exps of nums. We don't need
;; the names of the dirs, so all it remains is something like
;; (14848514 8504156 (29116 2557))
(defn process-text-line [line]
  (cond
    (= line "$ ls") nil
    (st/starts-with? line "dir") nil
    (st/starts-with? line "$ cd ..") ")"
    (st/starts-with? line "$ cd ") "("
    :else (let [[size _] (st/split line #" ")] size)))

(defn add-last-two [nums]
    (conj (->> nums pop pop) ;o)
          (->> nums (take-last 2) (reduce +))))

(defn token-reducer [[stack nums] token]
  (case token
    "(" [(conj stack 0) nums]
    ")" (if (= 1 (count stack))
          (reduced [[] (conj nums (first stack))])
          (let [lst (last stack)]
            [(add-last-two stack)
             (conj nums lst)]))
    [(conj (pop stack) (+ (parse-int token) (last stack)))
     nums]))

(defn process-clean-input [tokens]
  (reduce token-reducer [[][]] tokens))

(defn process-raw-input [input]
  (->> input
       (map process-text-line)
       (filter identity)
       ((fn [tokens] (concat tokens (repeat 20 ")")))) ;-) https://cutt.ly/Z0qbbkP
       process-clean-input
       second))

(defn find-size-min-dir [minimum sizes]
  (let [target (- (apply max sizes) minimum)]
    (->> sizes
         (filter #(>= % target))
         (apply min))))

(defn d7-read-sizes [& [filename]]
  (->> (read-input-day (or filename "d7"))
       process-raw-input))

(defn d7p1 [& [filename]]
  (->> (d7-read-sizes filename)
       (filter #(<= (or % 0) 100000))
       (reduce +)))

(defn d7p2 [& [filename]]
  (->> (d7-read-sizes filename)
       (find-size-min-dir 40000000)))

(deftest d07
  (is (= 95437 (d7p1 "d7-test")))
  (is (= 1086293 (d7p1)))
  (is (= 24933642 (d7p2 "d7-test")))
  (is (= 366028 (d7p2))))

;; Day 8
(defn visible-trees-row-front [row]
  (loop [[[_ _ val :as front] & rest] row
         max -1
         visible #{}]
    (if (nil? front)
      visible
      (if (< max val)
        (recur rest val (conj visible front) )
        (recur rest max visible)))))

(defn visible-trees-row [row]
  (set/union (visible-trees-row-front row)
             (visible-trees-row-front (reverse row))))

(defn visible-trees-rows [rows]
  (->> rows
       (map visible-trees-row)
       (apply set/union)))

(defn visible-trees [rows]
  (set/union (visible-trees-rows rows)
             (visible-trees-rows (apply map vector rows))))

(defn take-while-and
  "Like take-while, but it can also take the first element that doesn't meet the
  first predicate if it meets the second predicate"
  [pred pred2 col]
  (reduce (fn [acc n] (if (pred n)
                        (conj acc n)
                        (reduced (if (pred2 n)
                                   (conj acc n)
                                   acc))))
          []
          col))

(defn in-visible-distance [height row]
  (->> row
       (take-while-and #(<= (nth % 2) height) #(= (nth % 2) height))
       count))

(defn tree-score [rows [x y :as tree]]
  (let [height (-> rows (nth x) (nth y) (nth 2))
        left (->> (nth rows y) (take x) reverse (in-visible-distance height))
        right (->> (nth rows y) (drop x) (in-visible-distance height))
        up (->> rows (map #(nth % x)) (take y) reverse (in-visible-distance height))
        down (->> rows (map #(nth % x)) (drop y) (in-visible-distance height))]
    (* left right up down)))

(defn max-tree-score [rows]
  (apply max
         (for [i (range (- (count rows)  2))
               j (range (- (count (first rows)) 2))]
           (tree-score rows [(inc i) (inc j)]))))

(defn parse-d8 [rownum row]
  (vec (map-indexed (fn [idx it] [rownum idx (- (int it) (int \0))]) row)))

(defn d8p1 [& [filename]]
  (->> (read-input-day (or filename "d8"))
       (map-indexed parse-d8)
       visible-trees
       count))

(defn d8p2 [& [filename]]
  (->> (read-input-day (or filename "d8"))
       (map-indexed parse-d8)
       max-tree-score))

(deftest d08
  (is (= 21 (d8p1 "d8-test")))
  (is (= 1533 (d8p1 "d8")))
  (is (= 8 (d8p2 "d8-test")))
  (is (= 1 (d8p2 "d8"))))

(def foo
  (->> (read-input-day "d8-test")
       (map-indexed parse-d8)))
(tree-score foo [3 4])

;; Day 9
(defn update-tail-position [[nhx nhy :as new-head-position]
                            [otx oty :as old-tail-position]]
  (let [dx (- nhx otx)
        dy (- nhy oty)
        touching? (and (< (abs dx) 2) (< (abs dy) 2))
        incfn (fn [d] (cond (or (= d 0) touching?) 0
                            :else (/ d (abs d))))]
    [(+ otx (incfn dx)) (+ oty (incfn dy))]))

(def moves-d9 {"L" [-1 0] "R" [1 0] "U" [0 1] "D" [0 -1]})

(defn apply-move-d9 [{:keys [head tail tails]} [mx my :as move]]
  (let [[hx hy] head
        new-head [(+ mx hx) (+ my hy)]
        new-tail (update-tail-position new-head tail)]
    {:head new-head :tail new-tail :tails (conj tails new-tail)}))

(defn update-chain [acc n]
  (conj acc (update-tail-position (last acc) n)))

(defn update-knots [{:keys [knots tails]} [mx my :as move]]
  (let [[hx hy] (first knots)
        new-head [(+ hx mx) (+ hy my)]
        new-knots (reduce update-chain [new-head] (rest knots))]
    {:knots new-knots :tails (conj tails (last new-knots))}))

(defn parse-d9 [s]
  (let [[move times] (st/split s #" ")]
    (repeat (parse-int times) (moves-d9 move))))

(defn d9p1 [& [filename]]
  (->> (read-input-day (or filename "d9") parse-d9)
       (apply concat)
       (reduce apply-move-d9 {:head [0 0] :tail [0 0] :tails #{}})
       :tails
       count))
(defn d9p2 [& [filename]]
  (->> (read-input-day (or filename "d9") parse-d9)
       (apply concat)
       (reduce update-knots {:knots (repeat 10 [0 0]) :tails #{}})
       :tails
       count))

(deftest d09
  (is (= [0 0] (update-tail-position [1 1] [0 0])))
  (is (= [1 1] (update-tail-position [2 1] [0 0])))
  (is (= [-1 -1] (update-tail-position [-2 -1] [0 0])))
  (is (= (d9p1 "d9-test1") 13))
  (is (= (d9p1) 6181))
  (is (= (d9p2 "d9-test2") 36))
  (is (= (d9p2) 2386)))

;; Day 10
(defn exec [state [op arg]]
  (let [[[_ _ x] cycle] (or (last state) [[1 1 1] 0])]
    (cond
      (= op "noop") (conj state [[x x x] (inc cycle) "noop"])
      (= op "addx") (-> state
                        (conj [[x x x] (inc cycle)])
                        (conj [[x x (+ x arg)] (+ 2 cycle)])))))

(defn sum-idxs [sts]
  (let [f (fn [sts id] (* id (->> id (nth sts) first second)))]
    (+ (f sts 20)
       (f sts 60)
       (f sts 100)
       (f sts 140)
       (f sts 180)
       (f sts 220))))

(defn sprite-in-draw-position? [idx cycle]
  (let [col (mod idx 40)
        pos (-> cycle first second)]
    (< (abs (- pos col)) 2)))

(defn parse-d10 [s]
  (let [[op arg] (st/split s #" ")]
    [op (and arg (parse-int arg))]))

(defn d10p1 [& [filename]]
  (->> (read-input-day (or filename "d10") parse-d10)
       (reduce exec [[[1 1 1] 0]])
       sum-idxs))

(defn d10p2 [& [filename]]
  (->> (read-input-day (or filename "d10") parse-d10)
       (reduce exec [[[1 1 1] 0]])
       (drop 1)
       (map-indexed sprite-in-draw-position?)
       (map (fn [b] (if b "#" " ")))
       (partition 40)
       (map (fn [ss] (apply str ss)))))

(deftest d10
  (is (= (d10p1 "d10-test") 13140))
  (is (= (d10p1) 17380))
  (is (= (-> (d10p2) first (nth 5)) \space))
  (is (= (-> (d10p2) second (nth 2)) \space))
  (is (= (-> (d10p2) second (nth 5)) \#)))


;; Day 11
(defn str-split-words [s] (#(st/split s #"[ ]+")))

(defn parse-section-monkey [ndrop sec]
  (->> sec
       str-split-words
       (drop ndrop)))

(defn parse-monkey [[_ items operation test if-true if-false]]
  {:items     (->> items
                   (parse-section-monkey 3)
                   (map #(st/replace % #"," ""))
                   (map parse-int)
                   (map bigint)
                   vec)
   :operation (->> operation
                   (parse-section-monkey 5)
                   ((fn [[op arg]] [(if (= "+" op) + *)
                                    (if (= arg "old") nil (parse-int arg))])))
   :test      (->> test
                   (parse-section-monkey 4)
                   first
                   parse-int)
   :if-true   (->> if-true
                   (parse-section-monkey 6)
                   first
                   parse-int)
   :if-false  (->> if-false
                   (parse-section-monkey 6)
                   first
                   parse-int)
   :inspections    (bigint 0)})

(defn monkey-iteration [div-by-3? monkeys idx]
  (let [curr        (-> monkeys (get idx))
        item        (-> curr :items first)
        [op arg]    (-> :operation curr)
        op-result   (op item (or arg item))
        div         (if div-by-3? (quot op-result 3)
                        (rem op-result (->> monkeys (map :test) (reduce *))))
        ;div         (rem div (->> monkeys (map :test) (reduce *)))
        test-result (= 0 (mod div (:test curr)))
        new-mk-idx  (if test-result (:if-true curr) (:if-false curr))]
    (-> monkeys
        (update-in [idx :items] #(subvec % 1))
        (update-in [new-mk-idx :items] #(conj % div)))))

(defn monkey-turn [div-3? monkeys idx]
  (let [items (-> monkeys (get idx) :items)]
    (->> items
         (reduce (fn [acc _] (monkey-iteration div-3? acc idx)) monkeys)
         (#(update-in % [idx :inspections] (fn [n] (+ n (count items))))))))

(defn monkey-round [div-3? monkeys]
  (let [ret (reduce-kv (fn [acc idx m] (monkey-turn div-3? acc idx)) monkeys monkeys)]
    ;(println (->> ret (map :items) (map count) (reduce +)))
    ret))

(defn d11-common [div-3? niter & [filename]]
  (let [monkeys (->> (read-input-day (or filename "d11"))
                     (partition-by #(= % ""))
                     (filter #(not= % '("")))
                     (map parse-monkey)
                     vec)]
    (->> (reduce (fn [acc n] (monkey-round div-3? acc)) monkeys (range niter))
         (map :inspections)
         sort
         (take-last 2)
         (apply *))))

(defn d11p1 [& [filename]]
  (d11-common true 20 filename))

(defn d11p2 [& [filename]]
  (d11-common true 10000 filename))

(deftest d11
  (is (= 10605 (d11p1 "d11-test")))
  (is (= 58786 (d11p1)))
  (is (= 2713310158 (d11p2 "d11-test")))
  #_(is (= 1 (d11p2))))

;; Day 12
(defn d12-read-input [& [filename]]
  (->> (read-input-day (or filename "d12"))
       (map vec)
       vec))

(defn map-indexedv [f coll]
  (vec (map-indexed f coll)))

(defn vaget [varr x y]
  (when (and (< -1 x (count varr))
             (< -1 y (count (first varr))))
    (-> varr (nth x) (nth y))))

(defn afind-all [varr val]
  (->> varr
       (map-indexedv (fn [i row] (map-indexedv (fn [j val] [[i j] val]) row)))
       (apply concat)
       (filter (fn [[_ v]] (= v val)))))

(defn afind [varr val]
  (->> (afind-all varr val) first first))

(defn filter-children [varr [_ val _] [[i j] cval steps :as candidate]]
  (and cval ; cval is not nil
       val
       (not steps) ; candidate is not visited
       (<= (- (int cval) (int val)) 1)))

(defn children-nodes [varr [i j]]
  (->> [[(dec i) j] [(inc i) j] [i (dec j)] [i (inc j)]]
       (filter (fn [[x y]] (filter-children varr (vaget varr i j) (vaget varr x y))))))

(defn dijkstra-distance [varr [sx sy :as start] [ex ey :as end] children-fn]
  (let [varr (map-indexedv (fn [i row]
                             (map-indexedv (fn [j val] [[i j] val nil])
                                           row))
                           varr)]
    (loop [steps 1
           to-check [start]
           varr varr]
      (let [children (->> to-check
                          (mapcat (partial children-fn varr))
                          set
                          vec)]
        (cond
          (= 0 (count children)) nil
          (in? children end) steps
          :else (recur (inc steps)
                       children
                       (reduce (fn [varr [i j :as n]] (update-in varr n
                                                                 (fn [[coord val _]]
                                                                   [coord val steps])))
                               varr
                               children)))))))


(defn d12p1 [& [filename]]
  (let [input (d12-read-input (or filename "d12"))
        start (afind input \S)
        end (afind input \E)
        input (->> input (map (fn [row] (map (fn [c] (case c \S \a \E \z c)) row))))]
    (dijkstra-distance input start end children-nodes)))

(defn d12p2 [& [filename]]
  (let [input (d12-read-input (or filename "d12"))
        start (afind input \S)
        end (afind input \E)
        input (->> input (map (fn [row] (map (fn [c] (case c \S \a \E \z c)) row))))
        starts (afind-all input \a)
        starts (map first starts)]
    (->> starts
         (map #(dijkstra-distance input % end children-nodes))
         (filter #(not (nil? %)))
         (apply min))))

(deftest d12
  (is (= 31 (d12p1 "d12-test")))
  (is (= 350 (d12p1)))
  (is (= 29 (d12p2 "d12-test")))
  (is (= 349 (d12p2))))
