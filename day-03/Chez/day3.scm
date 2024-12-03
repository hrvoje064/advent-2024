;;; Advent of code 2024 - day3, part1 & 2, on BPI-F3 RISC-V
;;; Chez code 

(load "../../utils.so")

(define (string-parse s)
  (let ((len (string-length s)))
    (cond
     ((number? (string->number s))
      (let ((n (string->number s)))
	(if (exact? n) n "")))
     ((>= len 3)
      (let ((ns (list->string (drop (- len 3) (string->list s)))))
	(if (string=? ns "mul") "mul" "")))
     ((= (string-length s) 1) s)
     (else ""))))
	
(define (list-add l)
  (cond
   ;;((or (null? l) (null? (cdr l)) (null? (cddr l))) 0)
   ((< (length l) 6) 0)
   ((and (equal? (car l) "mul")
	 (string=? (second l) "(") (number? (third l))
	 (string=? (fourth l) ",") (number? (fifth l))
	 (string=? (sixth l) ")"))
    (+ (* (third l) (fifth l))
       (list-add (drop 6 l))))
   (else (list-add (cdr l)))))

(define (parse-donts chl)
  (letrec ((parse
	    (lambda (dos n cl)
	      (cond
	       ((< n 8) (if dos cl '()))
	       ((and (char=? (car cl) #\d) dos)
		(let-values ([(a b) (take-drop 7 cl)])
		  (if (string=? (list->string a) "don't()")
		      (parse #f (- n 7) b)
		      (cons (car cl)
			    (parse #t (sub1 n) (cdr cl))))))
	       ((and (char=? (car cl) #\d) (not dos))
		(let-values ([(a b) (take-drop 4 cl)])
		  (if (string=? (list->string a) "do()")
		      (parse #t (- n 4) b)
		      (parse #f (sub1 n) (cdr cl)))))
	       (dos (cons (car cl) (parse #t (sub1 n) (cdr cl))))
	       (else
		(parse #f (sub1 n) (cdr cl)))))))
    (parse #t (length chl) chl)))

(define (day3 file)
  (let* ((raw (read-lines file))
	 (charl (string->list (apply string-append raw)))
	 (doss (list->string (parse-donts charl)))
	 (ls1 (map (lambda (s) (string-split-parse s '(#\( #\) #\,))) raw))
	 (ls2 (string-split-parse doss '(#\( #\) #\,)))
	 (mul-n1
	  (map (lambda (l)
		 (map (lambda (s) (string-parse s)) l)) ls1))
	 (mul-n2 (map string-parse ls2)))
    (values (format "part1: ~a" (apply + (map list-add mul-n1)))
	    (format "part2: ~a" (list-add mul-n2)))))

