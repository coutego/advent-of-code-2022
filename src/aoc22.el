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
;; Homepage: https://github.com/pedroabelleiraseco/aoc22
;; Package-Requires: ((emacs "24.3"))
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
(defun aoc22-read-input-day (s &optional parse-line-fn)
  "Read the filename 's'.txt from resources.
It applies 'parse-line-fn' to all lines, if indicated."
  (let* ((fname (concat "../resources/aoc22/" s ".txt"))
         (ret   (s-lines (f-read fname)))
         (ret   (if parse-line-fn (-map parse-line-fn ret) ret)))
    ret))

(defun aoc22-parse-opt-int (s)
  (if (or (not s) (s-equals-p (s-trim s) ""))
      nil
    (string-to-number s)))

(defun aoc22-main ()
  (let* ((nums
          (cl-loop for i from 1 to 25
                   append (cl-loop for j from 1 to 2
                                   collect (list i j))))
         (fnames (-map (lambda (el) (concat "aoc22-d"
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
(defun aoc22-calories (nums)
  (->> (-partition-by #'not nums)
       (-filter (lambda (n) (not (-some #'not n))))
       (-map (lambda (ns) (apply #'+ ns)))))

(defun aoc22-sum-top-n (n nums)
  (->> (-sort #'> nums)
       (-take n)
       (-reduce #'+)))

(defun test-aoc22-d1 ()
 (let ((data '(1000 2000 3000 nil 4000 nil 5000 6000 nil 7000 8000 9000 nil 10000)))
   (cl-assert (= 24000 (apply #'max (aoc22-calories data))))
   (cl-assert (= 71934 (aoc22-d1p1)))
   (cl-assert (= 211447 (aoc22-d1p2)))))

(test-aoc22-d1)

(defun aoc22-d1p1 ()
  (->> (aoc22-read-input-day "d1p1" #'aoc22-parse-opt-int)
       aoc22-calories
       (apply #'max)))

(defun aoc22-d1p2 ()
  (->> (aoc22-read-input-day "d1p1" #'aoc22-parse-opt-int)
       aoc22-calories
       (aoc22-sum-top-n 3)))

(aoc22-d1p1)
(aoc22-d1p2)

(provide 'aoc22)
;;; aoc22.el ends here
