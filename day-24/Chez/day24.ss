;;; Advent of code 2024 - day24, part1 & part2, on BPI-F3 RISC-V
;;; Chez code

(load "utils.so")

(define table (make-hashtable string-hash string=? 256))

(define (make-function fs lw rw)
  (let ((LF (case fs
	      (("AND") logand)
	      (("XOR") logxor)
	      (else logor))))
    (letrec ((func
	      (lambda ()
		(let ((l (hashtable-ref table lw #f))
		      (r (hashtable-ref table rw #f)))
		  (if (and (number? l) (number? r))
		      (LF l r) func)))))
      func)))

(define (simulate keys zs zlen)
  (for-each (lambda (k)
	      (hashtable-update!
	       table k (lambda (v) (if (number? v) v (v))) #f)) keys)
  (let ((result
	 (filter (lambda (zk) (number? (hashtable-ref table zk #f))) zs)))
    (if (= (length result) zlen)
	(map (lambda (zk) (hashtable-ref table zk #f)) zs)
	(simulate keys zs zlen))))
			     
(define (day24 file)
  (hashtable-clear! table)
  (let-values
      ([(wires gates)
	(filter-split (lambda (s) (< (string-length s) 10))
		      (read-lines file))])
    (let ((wires
	   (map (lambda (sn) (list (car sn) (string->number (cadr sn))))
		(map (lambda (s) (string-split-n s '(#\: #\space)))
		     (reverse (cdr (reverse wires))))))
	  (gates
	   (map (lambda (gl)
		  (list (fourth gl)
			(make-function (second gl) (first gl) (third gl))))
		(map (lambda (g) (string-split-n g '(#\- #\> #\space))) gates))))
      (for-each (lambda (w) (hashtable-set! table (car w) (cadr w))) wires)
      (for-each (lambda (g) (hashtable-set! table (car g) (cadr g))) gates)
      (let* ((zs (sort string>? (filter (lambda (k) (char=? (string-ref k 0) #\z))
			 (vector->list (hashtable-keys table)))))
	     (zlength (length zs))
	     (part1
	      (simulate (vector->list (hashtable-keys table)) zs zlength)))
	(values (string->number
		 (fold-right
		  (lambda (p rs)
		    (string-append (number->string p) rs)) "" part1) 2)
		part1)))))
