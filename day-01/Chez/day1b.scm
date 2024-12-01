;;; Advent of code 2024 - day1, part1, on BPI-F3 RISC-V
;;; Chez Scheme code

(load "../../utils.so")

(define (day-1b file)
  (let-values ([(l r) (list-split (read-file file))])
    (apply
     + (map (lambda (x) (* x (length (filter (lambda (y) (= x y)) r))))
	    (sort < l)))))

