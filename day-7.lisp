(defpackage aoc-day-7
  (:use :cl)
  (:export :answers))
(in-package :aoc-day-7)

(defun parse-wire-instruction (circuit instruction)
  (let ((parts (reverse (uiop:split-string instruction)))
        wire-name)
    (setq wire-name (car parts))
    (setq parts (mapcar (lambda (s) (if (every 'digit-char-p s) (parse-integer s) s)) (reverse (cddr parts))))
    (setf (gethash wire-name circuit) parts)))

(defun eval-arg (circuit wires arg)
  (if (integerp arg)
      arg
      (let ((v (gethash arg wires)))
        (unless v
          (setq v (eval-wire circuit wires arg))
          (setf (gethash arg wires) v))
        v)))

(defun eval-wire (circuit wires wire-name)
  (let ((wire (gethash wire-name circuit))
        argc)
    (setq argc (length wire))
    (logand
      65535
      (cond
       ((= 1 argc) (eval-arg circuit wires (first wire)))
       ((= 2 argc)
         (progn (unless (equal "NOT" (first wire))
                  (error "invalid wire: ~A" wire))
                (logxor (eval-arg circuit wires (second wire)) 65535)))
       ((= 3 argc)
         (let ((gate (second wire))
               (lhs (eval-arg circuit wires (first wire)))
               (rhs (eval-arg circuit wires (third wire))))
           (cond
            ((equal "AND" gate) (logand lhs rhs))
            ((equal "OR" gate) (logior lhs rhs))
            ((equal "LSHIFT" gate) (ash lhs rhs))
            ((equal "RSHIFT" gate) (ash lhs (- rhs)))
            (t (error "invalid wire: ~A" wire)))))
       (t (error "invalid wire: ~A" wire))))))

(defun answers (input-file)
  (let ((input (uiop:read-file-lines input-file))
        (circuit (make-hash-table :test 'equal))
        (wires (make-hash-table :test 'equal)))
    (dolist (instruction input)
      (parse-wire-instruction circuit instruction))
    (let ((a-val (eval-wire circuit wires "a")))
      (setq wires (make-hash-table :test 'equal))
      (setf (gethash "b" wires) a-val)
      (cons a-val (eval-wire circuit wires "a")))))
