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

;;
;; General utility code
;;
(defun my-read-input-day (s &optional parse-line-fn skip-empty-lines)
  "Read the filename 's'.txt from resources.
It applies 'parse-line-fn' to all lines, if indicated."
  (let* ((fname (concat "../resources/aoc22/" s ".txt"))
         (ret   (s-lines (f-read fname)))
         (ret   (if parse-line-fn (-map parse-line-fn ret) ret))
         (ret   (if skip-empty-lines (-filter #'identity ret) ret)))
    ret))

(defun my-parse-opt-int (s)
  (if (or (not s) (s-equals-p (s-trim s) ""))
      nil
    (string-to-number s)))

(defun my-main ()
  (let* ((nums
          (cl-loop for i from 1 to 25
                   append (cl-loop for j from 1 to 2
                                   collect (list i j))))
         (fnames (-map (lambda (el) (concat "my-d"
                                            (number-to-string (car el))
                                            "p"
                                            (number-to-string (cadr el))))
                       nums)))
    (->> (-map
          (lambda (name)
            (let ((sym (intern name)))
              (when (fboundp sym) (funcall sym))))
          fnames)
         (-filter #'identity))))

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
  (->> (my-read-input-day "d1" #'my-parse-opt-int)
       my-calories
       (apply #'max)))

(defun my-d1p2 ()
  (->> (my-read-input-day "d1" #'my-parse-opt-int)
       my-calories
       (my-sum-top-n 3)))

(let ((data '(1000 2000 3000 nil 4000 nil 5000 6000 nil 7000 8000 9000 nil 10000)))
   (cl-assert (= 24000 (apply #'max (my-calories data)))))
(cl-assert (= 71934 (my-d1p1)))
(cl-assert (= 211447 (my-d1p2)))

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

(defun my-d2p1 () (-> (my-read-input-day "d2" #'my-parse-play t) my-score-strategy))
(defun my-d2p2 () (-> (my-read-input-day "d2" #'my-parse-play-win-lose t) my-score-strategy))

(cl-assert (= 8 (my-score-play "A" "B")))
(cl-assert (= 1 (my-score-strategy '(("B" "A")))))
(cl-assert (= 6 (my-score-strategy '(("C" "C")))))
(cl-assert (= 15 (my-score-strategy '(("A" "Y") ("B" "X") ("C" "Z")))))
(cl-assert (= 11767 (my-d2p1)))
(cl-assert (= 13886 (my-d2p2)))


(message "Everything works")

(provide 'aoc22)
;;; aoc22.el ends here
