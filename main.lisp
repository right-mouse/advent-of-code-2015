(load "day-1.lisp")

(defpackage aoc-main
  (:use :cl
        :aoc-day-1))
(in-package :aoc-main)

(defun main ()
  (format t "~A~%" (aoc-day-1::part-1)))
