(load "day-1.lisp")
(load "day-2.lisp")
(load "day-3.lisp")
(load "day-4.lisp")
(load "day-5.lisp")
(load "day-6.lisp")
(load "day-7.lisp")
(load "day-8.lisp")

(defpackage aoc
  (:use :cl))
(in-package :aoc)

(defun main (day)
  (let ((input (format nil "inputs/day-~A.txt" day)) results)
    (setq results
        (cond
         ((= day 1) (aoc-day-1:answers input))
         ((= day 2) (aoc-day-2:answers input))
         ((= day 3) (aoc-day-3:answers input))
         ((= day 4) (aoc-day-4:answers input))
         ((= day 5) (aoc-day-5:answers input))
         ((= day 6) (aoc-day-6:answers input))
         ((= day 7) (aoc-day-7:answers input))
         ((= day 8) (aoc-day-8:answers input))
         (t (error "~A is not a valid day" day))))
    (format t "Part 1: ~A~%" (car results))
    (format t "Part 2: ~A~%" (cdr results))))
