;;; Advent of code 2024 - day18, part1 & part2, on BPI-F3 RISC-V
;;; Chez code  --- Day 18: RAM Run ---

(load "utils.so")

(define ram #f) ; RAM matrix
(define table (make-hashtable equal-hash equal? 131072))
(define mxlimit #f)
(define todrop #f)

(define (init-ram n)
  (make-matrix n n))

(define (drop-bytes bytes n)
  (cond ((zero? n) 'done)
	(else
	 (matrix-set! ram (caar bytes) (cdar bytes) #f)
	 (drop-bytes (cdr bytes) (sub1 n)))))

(define (neighbors from)
  (filter values
	  (list (north from) (east from)
		(south from) (west from))))

(define (east from)
  (let ((x (add1 (car from))) (y (cdr from)))
    (and (<= x mxlimit) (matrix-ref ram x y) (cons x y))))

(define (west from)
  (let ((x (sub1 (car from))) (y (cdr from)))
    (and (>= x 0) (matrix-ref ram x y) (cons x y))))

(define (south from)
  (let ((x (car from)) (y (add1 (cdr from))))
    (and (<= y mxlimit) (matrix-ref ram x y) (cons x y))))

(define (north from)
  (let ((x (car from)) (y (sub1 (cdr from))))
    (and (>= y 0) (matrix-ref ram x y) (cons x y))))

(define (run seen from to)
  (cond
   ((equal? from to) '(0))
   (else
    (let ((nbrs (neighbors from)))
      (let-values ([(not-seen new-seen) (check-seen nbrs seen)])
	(flatten
	 (map (lambda (nbr) (inc1 (run new-seen nbr to)))
	      not-seen)))))))

(define (check-seen new old)
  (cond
   ((null? new) (values '() old))
   (else
    (let-values ([(n o) (check-seen (cdr new) old)])
      (if (member (car new) old)
	  (values n o)
	  (values (cons (car new) n) (cons (car new) o)))))))

(define (inc1 x)
  (cond ((pair? x)
	 (let ((r (map add1 (filter number? x))))
	   (if (null? r) #f (apply min r))))
	(else #f)))

(define (run2 ram1 bytes low high)
  (let* ((n (quotient (- high low) 2))
	 (m (+ low n)))
    (drop-bytes bytes m)
    (cond
     ((zero? n) (list-ref bytes m))
     (else
      (hashtable-clear! table)
      (cond
       ((find values (run '((0 . 0)) '(0 . 0) (cons mxlimit mxlimit)))
	(set! ram (mx-copy ram1))
	(run2 ram1 bytes m high))
       (else
	(set! ram (mx-copy ram1))
	(run2 ram1 bytes low m)))))))
  
(define (day18 file)
  (hashtable-clear! table)
  (if (string=? file "input.txt")
      (begin (set! mxlimit 70) (set! todrop 1024))
      (begin (set! mxlimit 6) (set! todrop 12)))
  (let ((bytes (map (lambda (s)
		      (apply cons (map string->number (string-split s #\,))))
		    (read-lines file))))
    (set! ram (init-ram (add1 mxlimit)))
    (let ((post (drop todrop bytes)))
      (drop-bytes bytes todrop)
      (let ((part1 (run '((0 . 0)) '(0 . 0) (cons mxlimit mxlimit))))
	(let ((part2 (run2 (mx-copy ram) post 0 (length post))))
	  (values (cadr part1)
		  (string-append (number->string (car part2)) ","
				 (number->string (cdr part2)))))))))

;;; memoize 
;;; ================================================================

(define (memoize-run f)
  (lambda (x from z)
    (let* ((visited (take 4 x))
	   (seen (hashtable-ref table (cons visited from) #f)))
      (or seen
          (let ((result (f x from z)))
            (hashtable-set! table (cons visited from) result)
            result)))))

(set! run (memoize-run run))

