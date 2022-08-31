(defpackage aoc-day-4
  (:use :cl)
  (:export :answers))
(in-package :aoc-day-4)

(ql:quickload "md5")

(defun check-hash-5-zeroes (hash)
  (and
   (= 0 (first hash))
   (= 0 (second hash))
   (= 0 (logand (third hash) #b11110000))))

(defun check-hash-6-zeroes (hash)
  (and
   (= 0 (first hash))
   (= 0 (second hash))
   (= 0 (third hash))))

(defun answers (input-file)
  (let ((input (uiop:read-file-string input-file))
        (i 0)
        five-zero-hash-num six-zero-hash-num)
    (loop
     (let ((hash (coerce (md5:md5sum-string (format nil "~A~A" input i)) 'list)))
       (unless five-zero-hash-num
         (when (check-hash-5-zeroes hash) (setq five-zero-hash-num i)))
       (unless six-zero-hash-num
         (when (check-hash-6-zeroes hash) (setq six-zero-hash-num i)))
       (when (and five-zero-hash-num six-zero-hash-num) (return))
       (incf i)))
    (cons five-zero-hash-num six-zero-hash-num)))
