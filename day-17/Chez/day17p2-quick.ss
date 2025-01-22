;;; Advent of code 2024 - day17, part1 & part2, on BPI-F3 RISC-V
;;; Chez code  --- Chronospatial Computer ---

(load "utils.so")

(define A #f)
(define B #f)
(define C #f)
(define pointer 0)
(define out "")
;;(define *init-A* (quotient (expt 8 16) 2))
;;(define *init-A* 35184372088828)
;;(define *init-A* 35184372088832) ; apsolute min
;;(define *init-A* 117400)
(define *init-A* 46337277)

(define operandv (make-vector 7))
(define opcodev (make-vector 8))
(define programv (make-vector 16))
(define programl '())

(define (init-operands o)
  (case o
    ((0 1 2 3) (vector-set! operandv o (lambda () o)))
    ((4) (vector-set! operandv o (lambda () A)))
    ((5) (vector-set! operandv o (lambda () B)))
    ((6) (vector-set! operandv o (lambda () C)))
    (else (error 'init-operands "illegal operand"))))

(define (init-opcode opc)
  (case opc
    ((0) (vector-set! opcodev opc
		      (lambda (o)
			(set! A (quotient A (expt 2 ((vector-ref operandv o)))))
			(jump 2))))
    ((1) (vector-set! opcodev opc
		      (lambda (o)
			(set! B (bitwise-xor B o)) (jump 2))))
    ((2) (vector-set! opcodev opc
		      (lambda (o)
			(set! B (modulo ((vector-ref operandv o)) 8)) (jump 2))))
    ((3) (vector-set! opcodev opc
		      (lambda (o)
			(if (not (zero? A))
			    (set! pointer o)
			    (jump 2)))))
    ((4) (vector-set! opcodev opc
		      (lambda (o)
			(set! B (bitwise-xor B C)) (jump 2))))
    ((5) (vector-set! opcodev opc
		      (lambda (o)
			(set! out (string-append
				   out ","
				   (number->string
				    (modulo ((vector-ref operandv o)) 8))))
			(jump 2))))
     ((6) (vector-set! opcodev opc
		       (lambda (o)
			 (set! B (quotient A (expt 2 ((vector-ref operandv o)))))
			 (jump 2))))
     ((7) (vector-set! opcodev opc
		       (lambda (o)
			 (set! C (quotient A (expt 2 ((vector-ref operandv o)))))
			 (jump 2))))
     (else (error 'init-opcode "invalid opcode"))))

(define (jump n) (set! pointer (+ pointer n)))

(define (run)
  (cond
   ((>= pointer (vector-length programv))
    (substring out 1 (string-length out)))
   (else
    ((vector-ref opcodev (vector-ref programv pointer))
     (vector-ref programv (add1 pointer)))
    (run))))

(define (run-test)
  (cond
   ((>= pointer (vector-length programv))
    (substring out 1 (string-length out)))
   (else
    (write (list "opc" (vector-ref programv pointer)
		 "opr" (vector-ref programv (add1 pointer))
		 "A" A "B" B "C" C "out" out))
    (newline)
    ((vector-ref opcodev (vector-ref programv pointer))
     (vector-ref programv (add1 pointer)))
    (run-test))))

(define (quick-find initA programl)
  (let ((a initA))
    (letrec
	((find-all
	  (lambda (prgl)
	    (let* ((b (bitwise-xor (modulo a 8) 1))
		   (c (quotient a (expt 2 b))))
	      (let ((b (bitwise-xor (bitwise-xor b c) 4)))
		(cond
		 ((null? prgl) initA)
		 ((= (modulo b 8) (car prgl))
		  (set! a (quotient a 8))
		  (find-all (cdr prgl)))
		 (else #f)))))))
      (or (find-all programl)
	  (quick-find (add1 initA) programl)))))

(define (correct? initA programl)
  (let ((a initA))
    (letrec
	((find-all
	  (lambda (prgl)
	    (let* ((b (bitwise-xor (modulo a 8) 1))
		   (c (quotient a (expt 2 b))))
	      (let ((b (bitwise-xor (bitwise-xor b c) 4)))
		(cond
		 ((null? prgl) initA)
		 ((= (modulo b 8) (car prgl))
		  (set! a (quotient a 8))
		  (find-all (cdr prgl)))
		 (else #f)))))))
      (find-all programl))))

(define (quick-findn initA programl n)
  (let ((a initA))
    (letrec
	((find-all
	  (lambda (prgl)
	    (let* ((b (bitwise-xor (modulo a 8) 1))
		   (c (quotient a (expt 2 b))))
	      (let ((b (bitwise-xor (bitwise-xor b c) 4)))
		(cond
		 ((null? prgl) initA)
		 ((= (modulo b 8) (car prgl))
		  (set! a (quotient a 8))
		  (find-all (cdr prgl)))
		 (else #f)))))))
      (if (zero? n)
	  #f
	  (or (find-all programl)
	      (quick-findn (add1 initA) programl (sub1 n)))))))

(define (search low high programl)
  (if (>= low high)
      #f
      (let ((mid (quotient (+ low high) 2)))
	(or
	 (quick-findn mid programl 100)
	 (search low mid programl)
	 (search (+ mid 100) high programl)))))   

(define (day17 file)
  (set! out "")
  (let-values ([(registers program) (take-drop 3 (read-lines file))])
    (let* ((registers (map string-split registers))
	   (program
	    (map string->number (cdr (string-split-n (cadr program) '(#\space #\,))))))
      (set! A (string->number (third (car registers))))
      (set! B (string->number (third (cadr registers))))
      (set! C (string->number (third (third registers))))
      (set! pointer 0)
      (for-each init-operands (iota 7))
      (for-each init-opcode (iota 8))
      (set! programv (list->vector program))
      (values (run) (buildA '(0) (reverse '(2 4 1 1 7 5 4 4 1 4 0 3 5 5 3 0)))))))

;;; =================================================

(define (buildA a rprgl)
  (cond
   ((null? rprgl) (car a))
   ((null? a) #f)
   ((> (length a) 1)
    (or (buildA (guess (* (car a) 8) 8 (car rprgl))
		(cdr rprgl))
	(buildA (cdr a) rprgl)))
    (else
     (buildA (guess (* (car a) 8) 8 (car rprgl))
	     (cdr rprgl)))))

(define (guess initA n out)
  (if (zero? n)
      '()
      (let* ((a initA )
	     (b (bitwise-xor (modulo a 8) 1))
	     (c (quotient a (expt 2 b))))
	(let ((b (bitwise-xor (bitwise-xor b c) 4)))
	  (if (= (modulo b 8) out)
	      (cons a (guess (add1 a) (sub1 n) out))
	      (guess (add1 a) (sub1 n) out))))))

;; (define (guess initA n out)
;;   (let ((a initA))
;;     (cond
;;      ((zero? n) '())
;;      ((= (modulo
;; 	  (bitwise-xor
;; 	   (bitwise-xor
;; 	    (bitwise-xor (modulo a 8) 1)
;; 	    (quotient a (expt 2 (bitwise-xor (modulo a 8) 1)))
;; 	    4)) 8) out)
;;       (cons a (guess (add1 a) (sub1 n) out)))
;;      (else (guess (add1 a) (sub1 n) out)))))

