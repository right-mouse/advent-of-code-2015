(defpackage aoc-day-5
  (:use :cl)
  (:export :answers))
(in-package :aoc-day-5)

(defun is-nice-old (word)
  (if (or (search "ab" word)
          (search "cd" word)
          (search "pq" word)
          (search "xy" word))
      nil
      (let ((prev-char #\Nul)
            (num-vowels 0)
            has-repeated-char)
        (map 'nil (lambda (c)
                    (unless has-repeated-char
                      (when (char= c prev-char) (setq has-repeated-char t)))
                    (when (or (char= c #\a)
                              (char= c #\e)
                              (char= c #\i)
                              (char= c #\o)
                              (char= c #\u))
                          (incf num-vowels))
                    (setq prev-char c))
          word)
        (and has-repeated-char (>= num-vowels 3)))))

(defun is-nice-new (word)
  (let ((i 1)
        substr
        has-repeated-pair
        has-repeated-char)
    (loop
     (unless has-repeated-pair
       (setq substr (subseq word (1- i) (1+ i)))
       (when (search substr word :start2 (1+ i))
             (setq has-repeated-pair t)))
     (unless has-repeated-char
       (when (char= (char word (1- i)) (char word (1+ i)))
             (setq has-repeated-char t)))
     (when (>= i (- (length word) 2)) (return))
     (incf i))
    (and has-repeated-pair has-repeated-char)))

(defun answers (input-file)
  (let ((input (uiop:read-file-lines input-file))
        nice-words-old
        nice-words-new)
    (setq nice-words-old (remove-if-not 'is-nice-old input))
    (setq nice-words-new (remove-if-not 'is-nice-new input))
    (cons (length nice-words-old) (length nice-words-new))))
