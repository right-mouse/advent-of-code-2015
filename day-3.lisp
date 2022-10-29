(defpackage aoc-day-3
  (:use :cl)
  (:export :answers))
(in-package :aoc-day-3)

(defun cons+ (a b)
  (cons (+ (car a) (car b)) (+ (cdr a) (cdr b))))

(defun answers (input-file)
  (let ((input (uiop:read-file-string input-file))
        (pos '(0 . 0)) (pos-santa '(0 . 0)) (pos-robot '(0 . 0))
        (visited '((0 . 0)))
        (santa-visited '((0 . 0)))
        (robot-visited '((0 . 0)))
        (santa-turn t))
    (map nil
        (lambda (c)
          (let ((delta (cond
                        ((char= c #\^) '(0 . 1))
                        ((char= c #\v) '(0 . -1))
                        ((char= c #\>) '(1 . 0))
                        ((char= c #\<) '(-1 . 0)))))
            (setq pos (cons+ pos delta))
            (setq visited (adjoin pos visited :test 'equal))
            (if santa-turn
                (progn
                 (setq pos-santa (cons+ pos-santa delta))
                 (setq santa-visited (adjoin pos-santa santa-visited :test 'equal)))
                (progn
                 (setq pos-robot (cons+ pos-robot delta))
                 (setq robot-visited (adjoin pos-robot robot-visited :test 'equal))))
            (setq santa-turn (not santa-turn))))
        input)
    (cons (length visited) (length (union santa-visited robot-visited :test 'equal)))))
