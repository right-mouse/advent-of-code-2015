(defpackage aoc-day-6
  (:use :cl)
  (:export :answers))
(in-package :aoc-day-6)

(defun do-simple-instruction (instruction lights)
  (let (split-parts
        start-coord end-coord
        x1 x2 y1 y2
        prefix-len
        new-val
        is-toggle)
    (cond
     ((uiop:string-prefix-p "turn off" instruction)
       (setq prefix-len 9))
     ((uiop:string-prefix-p "turn on" instruction)
       (progn (setq prefix-len 8)
              (setq new-val t)))
     ((uiop:string-prefix-p "toggle" instruction)
       (progn (setq prefix-len 7)
              (setq is-toggle t)))
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
                   do (setf (aref lights i j)
                        (if is-toggle
                            (not (aref lights i j))
                            new-val))))))

(defun do-elvish-instruction (instruction lights)
  (let (split-parts
        start-coord end-coord
        x1 x2 y1 y2
        prefix-len
        inc-val)
    (cond
     ((uiop:string-prefix-p "turn off" instruction)
       (progn (setq prefix-len 9)
              (setq inc-val -1)))
     ((uiop:string-prefix-p "turn on" instruction)
       (progn (setq prefix-len 8)
              (setq inc-val 1)))
     ((uiop:string-prefix-p "toggle" instruction)
       (progn (setq prefix-len 7)
              (setq inc-val 2)))
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
                   do (incf (aref lights i j) (max inc-val (- (aref lights i j))))))))

(defun count-on (lights)
  (let ((num 0))
    (destructuring-bind (n m) (array-dimensions lights)
      (loop for i from 0 below n
            do (loop for j from 0 below m
                     do (when (aref lights i j) (incf num)))))
    num))

(defun count-brightness (lights)
  (let ((num 0))
    (destructuring-bind (n m) (array-dimensions lights)
      (loop for i from 0 below n
            do (loop for j from 0 below m
                     do (incf num (aref lights i j)))))
    num))

(defun answers (input-file)
  (let ((input (uiop:read-file-lines input-file))
        (binary-lights (make-array '(1000 1000) :element-type 't :initial-element nil))
        (numeric-lights (make-array '(1000 1000))))
    (dolist (instruction input)
      (do-simple-instruction instruction binary-lights)
      (do-elvish-instruction instruction numeric-lights))
    (cons (count-on binary-lights) (count-brightness numeric-lights))))
