(defpackage aoc-day-1
  (:use :cl)
  (:export :answers))
(in-package :aoc-day-1)

(defun answers (input-file)
  (let ((input (uiop:read-file-string input-file))
        (pos 0)
        (cnt 0)
        enter-basement)
    (map nil (lambda (c)
               (when (null enter-basement) (incf pos))
               (cond
                ((char= c #\() (incf cnt))
                ((char= c #\)) (decf cnt)))
               (when (and (null enter-basement) (< cnt 0)) (setq enter-basement pos)))
        input)
    (cons cnt enter-basement)))
