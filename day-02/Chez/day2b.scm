;;; Advent of code 2024 - day2, part2, on BPI-F3 RISC-V
;;; Chez code 

(load "../../utils.so")

(define (safe-up l)
  (if (null? (cdr l))
      #t
      (and (< (car l) (cadr l))
	   (<= (- (cadr l) (car l)) 3)
	   (safe-up (cdr l)))))

(define (safe-down l)
  (if (null? (cdr l))
      #t
      (and (> (car l) (cadr l))
	   (<= (- (car l) (cadr l)) 3)
	   (safe-down (cdr l)))))

(define (safe? l)
  (if (< (car l) (cadr l))
      (safe-up l)
      (safe-down l)))

(define (dampener n l)
  (let-values ([(a b) (take-drop n l)])
    (if (null? b)
	#f
	(or (safe? (append a (cdr b)))
	    (dampener (add1 n) l)))))

(define (day2b file)
  (let ((ll (map (lambda (l) (map string->number l))
		  (read-lol-words file))))
    (length (filter (lambda (l) (dampener 0 l)) ll))))

