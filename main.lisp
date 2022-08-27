(load "day-1.lisp")

(defpackage aoc
  (:use :cl
        :aoc-day-1))
(in-package :aoc)

(defun main (day)
  (let ((input (format nil "inputs/day-~A.txt" day)) results)
    (cond
     ((= day 1)
       (setq results (aoc-day-1::answers input)))
     (t
       (error "~A is not a valid day" day)))
    (format t "Part 1: ~A~%" (car results))
    (format t "Part 2: ~A~%" (cdr results))))
