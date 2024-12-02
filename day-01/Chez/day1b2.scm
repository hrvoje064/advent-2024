;;; Advent of code 2024 - day1, part2, on BPI-F3 RISC-V
;;; Chez Scheme code - using BST as a histogram

(load "../../utils.so")

(define (day-1b file)
  (let-values ([(l r) (list-split (read-file file))])
    (let ((bst (bst-histogram r)))
      (apply + (map (lambda (x) (* x (bst-get 0 x bst))) l)))))
