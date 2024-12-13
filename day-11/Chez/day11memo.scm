;;; Advent of code 2024 - day11, part1 & part2, on BPI-F3 RISC-V
;;; Chez code - memoized-version)

(load "utils.so")

(define table (make-hashtable equal-hash equal?))

(define (memoize f)
  (lambda (x y)
    (let ((seen (hashtable-ref table (cons x y) #f)))
      (or seen
          (let ((result (f x y)))
            (hashtable-set! table (cons x y) result)
            result)))))

(define memo-rule
  (memoize
   (lambda (n g)
     (let ((bottom (cond ((zero? n) '(1))
			 (else
			  (let* ((ns (number->string n))
				 (sl (string-length ns)))
			    (if (even? sl)
				(let ((d (quotient sl 2)))
				  (list (string->number (substring ns 0 d))
					(string->number (substring ns d sl))))
				(list (* n 2024))))))))
       (if (zero? g)
	   (length bottom)
	   (if (null? (cdr bottom))
	       (memo-rule (car bottom) (sub1 g))
	       (+ (memo-rule (car bottom) (sub1 g))
		  (memo-rule (cadr bottom) (sub1 g)))))))))		       

(define (day11 file)
  (hashtable-clear! table)
  (let* ((line (read-file file))
	 (part1 (apply + (map (lambda (s) (memo-rule s 24)) line)))
	 (part2 (apply + (map (lambda (s) (memo-rule s 74)) line))))
    (values part1 part2)))

