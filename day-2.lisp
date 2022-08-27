(defpackage aoc-day-2
  (:use :cl)
  (:export :answers))
(in-package :aoc-day-2)

(defun answers (input-file)
  (let ((input (mapcar
                   (lambda (s) (sort (mapcar 'parse-integer (uiop:split-string s :separator "x")) '<))
                   (uiop:read-file-lines input-file)))
        lengths widths heights surface-area ribbon-length)
    (setq lengths (mapcar 'car input))
    (setq widths (mapcar 'cadr input))
    (setq heights (mapcar 'caddr input))
    (setq surface-area
        (apply '+
          (mapcar (lambda (x y z)
                    (let ((a1 (* x y))
                          (a2 (* y z))
                          (a3 (* x z)))
                      (+ (* 2 a1) (* 2 a2) (* 2 a3) (min a1 a2 a3))))
              lengths widths heights)))
    (setq ribbon-length
        (apply '+
          (mapcar (lambda (n)
                    (let ((smallest-side (subseq n 0 2)))
                      (+ (* 2 (apply '+ smallest-side)) (apply '* n))))
              input)))
    (cons surface-area ribbon-length)))
