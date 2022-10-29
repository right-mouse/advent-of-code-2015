(defpackage aoc-day-6
  (:use :cl)
  (:export :answers))
(in-package :aoc-day-6)

(defun do-instruction (instruction lights simple)
  (let (split-parts
        start-coord end-coord
        x1 x2 y1 y2
        prefix-len
        new-val is-toggle
        inc-val)
    (cond
     ((uiop:string-prefix-p "turn off" instruction)
       (progn (setq prefix-len 9)
              (unless simple (setq inc-val -1))))
     ((uiop:string-prefix-p "turn on" instruction)
       (progn (setq prefix-len 8)
              (if simple
                  (setq new-val t)
                  (setq inc-val 1))))
     ((uiop:string-prefix-p "toggle" instruction)
       (progn (setq prefix-len 7)
              (if simple
                  (setq is-toggle t)
                  (setq inc-val 2))))
     (t (error "invalid instruction: ~A" instruction)))
    (setq split-parts (uiop:split-string (subseq instruction prefix-len)))
    (setq start-coord (uiop:split-string (first split-parts) :separator ","))
    (setq end-coord (uiop:split-string (third split-parts) :separator ","))
    (setq x1 (parse-integer (first start-coord)))
    (setq y1 (parse-integer (second start-coord)))
    (setq x2 (parse-integer (first end-coord)))
    (setq y2 (parse-integer (second end-coord)))
    (loop for i from x1 to x2
          do (loop for j from y1 to y2
                   do (if simple
                          (setf (aref lights i j)
                            (if is-toggle (not (aref lights i j)) new-val))
                          (incf (aref lights i j) (max inc-val (- (aref lights i j)))))))))

(defun count-lights (lights)
  (let ((num 0))
    (destructuring-bind (n m) (array-dimensions lights)
      (loop for i from 0 below n
            do (loop for j from 0 below m
                     do (let ((l (aref lights i j)))
                          (when l (if (eq l t)
                                      (incf num)
                                      (incf num l)))))))
    num))

(defun answers (input-file)
  (let ((input (uiop:read-file-lines input-file))
        (binary-lights (make-array '(1000 1000) :element-type 't :initial-element nil))
        (numeric-lights (make-array '(1000 1000))))
    (dolist (instruction input)
      (do-instruction instruction binary-lights t)
      (do-instruction instruction numeric-lights nil))
    (cons (count-lights binary-lights) (count-lights numeric-lights))))
