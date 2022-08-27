(defpackage aoc-day-2
  (:use :cl)
  (:export :answers))
(in-package :aoc-day-2)

(defun answers (input-file)
  (let ((input (mapcar
                   (lambda (s) (sort (mapcar 'parse-integer (uiop:split-string s :separator "x")) '<))
                   (uiop:read-file-lines input-file)))
        surface-area ribbon-length)
    (setq surface-area
        (apply '+
          (mapcar (lambda (n)
                    (let ((x (car n))
                          (y (cadr n))
                          (z (caddr n)))
                      (+ (* 3 x y) (* 2 y z) (* 2 x z))))
              input)))
    (setq ribbon-length
        (apply '+
          (mapcar (lambda (n)
                    (let ((smallest-side (subseq n 0 2)))
                      (+ (* 2 (apply '+ smallest-side)) (apply '* n))))
              input)))
    (cons surface-area ribbon-length)))
