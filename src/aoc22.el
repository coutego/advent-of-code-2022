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
(defun aoc22-num-increases (nums)
  (->> (-zip nums (cdr nums))
       (-count (lambda (el) (< (car el) (cdr el))))))

(defun aoc22-three-window-sum (nums)
  (->> (-zip nums (cdr nums) (cddr nums))
       (-map (lambda (el) (-reduce #'+ el)))))

(defun aoc22-d1p1 ()
  (-> (aoc22-read-input-day "d1p1" #'string-to-number)
      aoc22-num-increases))

(defun aoc22-d1p2 ()
  (-> (aoc22-read-input-day "d1p1" #'string-to-number)
      aoc22-three-window-sum
      aoc22-num-increases))


(provide 'aoc22)
;;; aoc22.el ends here
