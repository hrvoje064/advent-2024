;;; Advent of code 2024 - day1, part1, on BPI-F3 RISC-V

(load "../../utils.so")

(define (day-1a file)
  (let-values ([(l r) (list-split (read-file file))])
    (apply
     + (map (lambda (x y) (abs (- x y))) (sort < l) (sort < r)))))


;; hrvoje@BPI-F3:~/Projects/advent-of-code/2024/day-01/Chez$ scheme day1a.scm
;; Chez Scheme Version 10.2.0-pre-release.1
;; Copyright 1984-2024 Cisco Systems, Inc.

;; > (time (lambda () (day-1a "input-1a.txt")))
;; 3508942
;; #<time-duration 0.005306273>




