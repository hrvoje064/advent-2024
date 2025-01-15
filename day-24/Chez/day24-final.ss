;;; Advent of code 2024 - day24, part1 & part2, on BPI-F3 RISC-V
;;; Chez code

(load "utils.so")

(define table (make-hashtable string-hash string=? 256))

(define swap-acc '(("z08" "ffj") ("dwp" "kfm") ("z22" "gjh") ("z31" "jdr")))

(define *gatesf* '())

(define (make-function lw fs rw)
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

(define (simulate x+yb keys zs zlen)
  (for-each (lambda (k)
	      (hashtable-update!
	       table k (lambda (v) (if (number? v) v (v))) #f)) keys)
  (if (= (length
	  (filter
	   (lambda (zk) (number? (hashtable-ref table zk #f))) zs)) zlen)
      (let* ((rbn (map (lambda (zk) (hashtable-ref table zk #f)) zs))
	     (z (check rbn x+yb zs)))
	(values rbn z))
      (simulate x+yb keys zs zlen)))

(define (check rbn x+yb zs)
  (cond
   ((null? rbn) '())
   ((= (car x+yb) (car rbn)) (check (cdr rbn) (cdr x+yb) (cdr zs)))
   (else (cons (list (car zs) (car x+yb) (car rbn))
	       (check (cdr rbn) (cdr x+yb) (cdr zs))))))
  
(define (swapm)
  (init-gatesf)
  (for-each
   (lambda (wp)
     (let ((w1t (hashtable-ref table (car wp) #f)))
       (hashtable-set! table (car wp) (hashtable-ref table (second wp) #f))
       (hashtable-set! table (second wp) w1t))) swap-acc))

(define (build-tree w)
  (let ((g (find (lambda (w1) (string=? (fourth w1) w)) *gates*)))
    (if g
	(cons g
	      (append (build-tree (car g))
		      (build-tree (third g))))
	'())))
       
(define (init-gatesf)
  (for-each (lambda (g) (hashtable-set! table (car g) (cadr g))) *gatesf*))

(define (day24 file)
  (hashtable-clear! table)
  (set! *gatesf* '())
  (let-values
      ([(wires gates)
	(filter-split (lambda (s) (< (string-length s) 10))
		      (read-lines file))])
    (let ((wires
	   (map (lambda (sn) (list (car sn) (string->number (cadr sn))))
		(map (lambda (s) (string-split-n s '(#\: #\space)))
		     (reverse (cdr (reverse wires)))))))
      (set! *gates*
	    (map (lambda (g) (string-split-n g '(#\- #\> #\space))) gates))
      (set! *gatesf* 
	    (map (lambda (gl)
		   (list (fourth gl)
			 (make-function (first gl) (second gl) (third gl)))) *gates*))
      (for-each (lambda (w) (hashtable-set! table (car w) (cadr w))) wires)
      (init-gatesf)
      (let* ((keys (vector->list (hashtable-keys table)))
	     (zs (sort string>? (filter (lambda (k) (char=? (string-ref k 0) #\z))
					keys)))
	     (xs (sort string>? (filter (lambda (k) (char=? (string-ref k 0) #\x))
					keys)))
	     (ys (sort string>? (filter (lambda (k) (char=? (string-ref k 0) #\y))
					keys)))
	     (xbn (map (lambda (k) (hashtable-ref table k #f)) xs))
	     (ybn (map (lambda (k) (hashtable-ref table k #f)) ys))
	     (x+yb (bin+ xbn ybn))
	     (zlength (length zs)))
	(let-values ([(part1 _) (simulate x+yb keys zs zlength)])
	  (swapm)
	  (let-values ([(_1 _2) (simulate x+yb keys zs zlength)])
	    (let* ((answer
		    (sort
		     string<?
		     (fold-right
		      (lambda (p r) (cons (car p) (cons (cadr p) r))) '() swap-acc)))
		   (ans-str
		    (fold-right
		     (lambda (w s) (string-append w "," s)) "" answer))
		   (final (if (string=? ans-str "")
			      ""
			      (substring ans-str 0 (sub1 (string-length ans-str))))))
	      (values (bin->dec part1) final))))))))
