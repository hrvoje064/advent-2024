;;; Advent of code 2024 - day22, part1 & part2, on BPI-F3 RISC-V
;;; Chez code

(load "utils.so")

(define (next sn)
  (let* ((sn1 (modulo (logxor sn (* sn 64)) 16777216))
	 (sn2 (modulo (logxor sn1 (quotient sn1 32)) 16777216)))
    (modulo (logxor sn2 (* sn2 2048)) 16777216)))
	 
(define (best-pr sn)
  (let ((acc '()))
    (do ([i 2001 (sub1 i)]
	 [n sn (next n)])
	((zero? i) (reverse acc))
      (set! acc (cons (modulo n 10) acc)))))

(define (next-sns sn)
  (set! acc '())
  (do ([i 2000 (sub1 i)]
       [n sn (next n)])
      ((zero? i) n)))

(define (diff l)
  (if (= (length l) 2)
      (list (- (second l) (car l)))
      (cons (- (second l) (car l))
	    (diff (cdr l)))))

(define (sequence seq lst)
  (let ((s (take 5 lst)))
    (cond
     ((< (length s) 5) 0)
     ((equal? (diff s) seq) (car (last-pair s)))
     (else (sequence seq (cdr lst))))))

(define (day22 file)
  (let* ((numbers (read-file file))
	 (part1 (apply + (map next-sns numbers)))
	 (bananas (map (lambda (n) (sequence '(0 0 -1 1) (best-pr n))) numbers)))
    (values part1 (apply + bananas))))
