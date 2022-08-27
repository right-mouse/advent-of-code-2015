(defpackage aoc-day-1
  (:use :cl)
  (:export :answers))
(in-package :aoc-day-1)

(defun answers (input-file)
  (let ((input (uiop:read-file-string input-file))
        (cnt 0)
        (pos 0)
        enter-basement)
    (map nil (lambda (c)
               (when (null enter-basement) (incf cnt))
               (cond
                ((char= c #\() (incf pos))
                ((char= c #\)) (decf pos)))
               (when (and (null enter-basement) (< pos 0)) (setq enter-basement cnt)))
        input)
    (cons pos enter-basement)))
