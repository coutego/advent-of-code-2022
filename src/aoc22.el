;;; aoc22.el --- Advent of Code 2022 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Coutego
;;
;; Author: Coutego <coutego@gmail.com>
;; Maintainer: Coutego <coutego@gmail.com>
;; Created: December 01, 2022
;; Modified: December 01, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/coutego/advent-of-code-2022
;; Package-Requires: ((emacs "27.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Advent of Code 2022
;;
;;; Code:

(require 'dash)
(require 'f)
(require 's)
(require 'cl-lib)
(require 'ert)

;;
;; General utility code
;;
(defun my-read-input-day (s &optional parse-line-fn keep-empty-lines?)
  "Read the filename 's'.txt from resources.
It applies 'parse-line-fn' to all lines, if indicated."
  (let* ((fname (concat "../resources/aoc22/" s ".txt"))
         (ret   (s-lines (f-read fname)))
         (ret   (if parse-line-fn (-map parse-line-fn ret) ret))
         (ret   (if (not keep-empty-lines?) (-filter #'identity ret) ret)))
    ret))

(defun my-parse-opt-int (s)
  (if (or (not s) (s-equals-p (s-trim s) ""))
      nil
    (string-to-number s)))

(defun my-run-all ()
  "Run all the functions for the different days"
  (interactive)
  (let* ((fnames (cl-loop for i from 1 to 25
                          append (cl-loop for j from 1 to 2
                                          collect (concat "my-d"
                                                          (number-to-string i)
                                                          "p"
                                                          (number-to-string j))))))
    (->> (--map (let ((sym (intern it)))
                  (when (fboundp sym)
                    (format "Result for day %s, part %s: %s\n"
                            (substring it 4 5)
                            (substring it 6 7)
                            (funcall sym))))
                fnames)
        (-filter #'identity)
        (apply #'concat)
        (message))))

(defun my-parse-int (s)
  (string-to-number s))

(defun my-run-all-tests ()
  (interactive)
  (ert t))

;;
;; Code
;;

;; Day 1
(defun my-calories (nums)
  (->> (-partition-by #'not nums)
       (-filter (lambda (n) (not (-some #'not n))))
       (-map (lambda (ns) (apply #'+ ns)))))

(defun my-sum-top-n (n nums)
  (->> (-sort #'> nums)
       (-take n)
       (-reduce #'+)))

(defun my-d1p1 ()
  (->> (my-read-input-day "d1" #'my-parse-opt-int t)
       my-calories
       (apply #'max)))

(defun my-d1p2 ()
  (->> (my-read-input-day "d1" #'my-parse-opt-int t)
       my-calories
       (my-sum-top-n 3)))

(ert-deftest my-erttest-d1 ()
  "Tests for day 1"
  (let ((data '(1000 2000 3000 nil 4000 nil 5000 6000 nil 7000 8000 9000 nil 10000)))
    (should (equal 24000 (apply #'max (my-calories data)))))
  (should (equal 71934 (my-d1p1)))
  (should (equal 211447 (my-d1p2))))

;; Day 2
(defvar my-plays '("A" "B" "C" "X" "Y" "Z"))
(defun my-play2n (p) (mod (-elem-index p my-plays) 3))
(defun my-result-play (p1 p2) (-> (- (my-play2n p2) (my-play2n p1)) (+ 1) (mod 3)))
(defun my-score-play (p1 p2) (+ (+ 1 (my-play2n p2)) (* 3 (my-result-play p1 p2))))
(defun my-score-strategy (st)
  (->> st (-map (-lambda ((p r)) (my-score-play p r))) (-reduce #'+)))
(defun my-parse-play (s) (if (s-equals? "" (s-trim s)) nil (s-split " " s)))

(defun my-parse-play-win-lose (s)
  (-let (((p r) (my-parse-play s)))
    (if (not p)
        nil
      (list p (-> r my-play2n (- 1) (+ (my-play2n p)) (mod 3) (nth my-plays))))))

(defun my-d2p1 () (-> (my-read-input-day "d2" #'my-parse-play) my-score-strategy))
(defun my-d2p2 () (-> (my-read-input-day "d2" #'my-parse-play-win-lose) my-score-strategy))

(ert-deftest my-erttest-d2 ()
  "Tests for day 2"
  (should (equal 8 (my-score-play "A" "B")))
  (should (equal 1 (my-score-strategy '(("B" "A")))))
  (should (equal 6 (my-score-strategy '(("C" "C")))))
  (should (equal 15 (my-score-strategy '(("A" "Y") ("B" "X") ("C" "Z")))))
  (should (equal 11767 (my-d2p1)))
  (should (equal 13886 (my-d2p2))))

;; Day 3
(defun my-priority (it) (if (> it 91) (- it 96) (- it 38))) ;; ascii codes of chars
(defun my-string-to-chars (s) (-map #'identity s))

(defun my-take-or-drop-half (t-d s)
  (-let ((s (my-string-to-chars s))
         (n (/ (length s) 2)))
    (if t-d (-take n s) (-drop n s))))

(defun my-half-n (n s) (-> (= 0 (mod n 2)) (my-take-or-drop-half s)))
(defun my-item-both-comp (rucksack) (car (cl-intersection (my-half-n 0 rucksack) (my-half-n 1 rucksack))))
(defun my-sum-priorities (rucksacks) (->> rucksacks (-map #'my-item-both-comp) (-map #'my-priority) (-reduce #'+)))
(defun my-common-item (rucksacks) (->> rucksacks (-map #'my-string-to-chars) (apply #'cl-intersection) car))
(defun my-sum-badges (groups) (->> groups (-map #'my-common-item) (-map #'my-priority) (-reduce #'+)))

(defun my-d3p1 () (->> (my-read-input-day "d3" #'identity) my-sum-priorities))
(defun my-d3p2 () (->> (my-read-input-day "d3" #'identity) (-partition 3) my-sum-badges))

;; (ert-deftest d03 ()
;;   "Tests for day 3"
;;   (should (equal '(97 98) (my-half-n 0 "abaZ")))
;;   (should (equal '(97 90) (my-take-or-drop-half nil "abaZ")))
;;   (should (equal ?a (my-item-both-comp "abaZ")))
;;   (should (equal 28 (my-sum-priorities ["abac" "aAbA"])))
;;   (let ((data '("vJrwpWtwJgWrhcsFMMfFFhFp"
;;                 "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
;;                 "PmmdzqPrVvPwwTWBwg"
;;                 "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
;;                 "ttgJtRGJQctTZtZT"
;;                 "CrZsJsPPZsGzwwsLwLmpwMDw")))
;;     (should (equal 157 (my-sum-priorities data)))
;;     (should (equal 70 (my-sum-badges (-partition 3 data)))))
;;   ;; (should (equal 7845 (my-d3p1)))
;;   ;; (should (equal 2790 (my-d3p2))))
;;   )



;; Day 4
(defun my-fully-contained? (s1 s2)
  (and (>= (car s1) (car s2))
       (<= (cadr s1) (cadr s2))))

(defun my-fully-overlap? (rs)
  (-let (((s1 s2) rs))
    (or (my-fully-contained? s1 s2)
        (my-fully-contained? s2 s1))))

(defun my-in-range? (r n)
  (-let (((a b) r))
    (and (<= a n)
         (<= n b))))

(defun my-range-in-range? (r1 r2)
  (-let (((a b) r2))
    (or (my-in-range? r1 a)
        (my-in-range? r1 b))))

(defun my-overlap? (rs)
  (-let (((s1 s2) rs))
    (or (my-range-in-range? s1 s2)
        (my-range-in-range? s2 s1))))

(defun my-parse-d4 (line)
  (->> (s-split "[,-]" line )
       (-map #'my-parse-int)
       (-partition 2)))

(defun d4p1 ()
  (->> (my-read-input-day "d4" #'my-parse-d4)
       (-filter #'my-fully-overlap?)
       seq-length))

(defun d4p2 ()
  (->> (my-read-input-day "d4" #'my-parse-d4)
       (-filter #'my-overlap?)
       seq-length))

(ert-deftest d04 ()
    "Test d04"
  (should (my-fully-contained? '(3 7) '(2 8)))
  (should (my-fully-overlap? '((2 8) (3 7)) ))
  ;; (should (equal 2
  ;;                (->> (my-read-input-day "d4-test" #'my-parse-d4)
  ;;                     (-filter fully-overlap?)
  ;;                     #'seq-length)))
  (should (equal 644 (d4p1)))
  ;; (should (equal 4 (->> (read-input-day "d4-test" parse-d4) (filter overlap?) #'seq-length)))
  (should (equal 926 (d4p2))))

(provide 'aoc22)

;; Local Variables:
;; read-symbol-shorthands: (("my-" . "aoc22-"))
;; End:
;; aoc22.el ends here
