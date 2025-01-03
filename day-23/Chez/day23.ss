;;; Advent of code 2024 - day23, part1 & part2, on BPI-F3 RISC-V
;;; Chez code

(load "utils.so")

(define htcon (make-hashtable string-hash string=? 6400))

(define (computers comps)
  (if (null? comps)
      '()
      (cons (caar comps)
	    (cons (cadar comps) (computers (cdr comps))))))

(define (find3s comps conncts)
  (if (null? comps)
      '()
      (let ((threes (find? (car comps) connects)))
	(if (and threes (> (length threes) 1))
	    (cons threes (find3s (cdr comps) conncts))
	    (find3s (cdr comps) conncts)))))

(define (t? s) (char=? (string-ref s 0) #\t))

(define (ts? conn) (t? (car conn)))

(define (tfirst conn)
  (if (t? (car conn)) conn (list (cadr conn) (car conn))))

(define (find x y xs)
  (cond
   ((null? xs) '())
   ((or (equal? (list x y) (car xs))
	(equal? (list y x) (car xs)))
    (car xs))
   (else
    (find x y (cdr xs)))))

(define (links tsxs allxs allts alltsxs)
  (letrec
      ((link
	(lambda (x xs)
	  (if (null? xs)
	      '()
	      (cons
	       (cons (caar tsxs) (find x (car xs) allxs))
	       (link x (cdr xs))))))
       (linkt
	(lambda (x xs)
	  (if (null? xs)
	      '()
	      (cons
	       (cons (caar tsxs) (find x (car xs) allts))
	       (linkt x (cdr xs)))))))
    (cond
     ((null? tsxs) '())
     ((t? (cadar tsxs))
      (append (linkt (cadar tsxs) (map cadr alltsxs))
	      (links (cdr tsxs) allxs allts alltsxs)))
     (else
      (append (link (cadar tsxs) (map cadr (cdr tsxs)))
	      (links (cdr tsxs) allxs allts alltsxs))))))

(define (lan-party computers)
  (fold-right
   (lambda (p r) (if (> (length p) (length r)) p r))
   '()
   (map
    (lambda (c)
      (let ((lvl1 (hashtable-ref htcon c #f)))
	(rem-dups-sort
	 (filter
	  (lambda (c2)
	    (< 12 (length (intersect-slow
			  lvl1
			  (hashtable-ref htcon c2 #f))))) lvl1))))
    computers)))

(define (day23 file)
  (hashtable-clear! htcon)
  (let* ((connections
	  (map (lambda (s) (sort string<? (string-split s #\-)))
	       (read-lines file)))
	 (connections1 (map tfirst connections)))
    (for-each
     (lambda (cn)
       (hashtable-update! htcon (car cn)
			  (lambda (v) (cons (cadr cn) (cons (car cn) v))) '())
       (hashtable-update! htcon (cadr cn)
			  (lambda (v) (cons (car cn) (cons (cadr cn) v))) '()))
     connections)
    (let*-values ([(ts xs) (filter-split ts? connections1)]
		  [(tcmp xcons)
		   (filter-split t? (rem-dups-sort (computers ts)))])
      (let* ((tpairs
	      (map (lambda (tc)
		     (filter (lambda (tp) (string=? tc (car tp))) ts)) tcmp))
	     (part1 (length
		     (rem-dups-sort
		      (filter (lambda (p) (> (length p) 1))
			      (apply
			       append
			       (map (lambda (tps) (links tps xs ts tps))
				    tpairs))))))
	     (part2 (fold-right (lambda (s ss) (string-append s "," ss)) ""
     				(lan-party (vector->list (hashtable-keys htcon))))))
	(values part1
		(substring part2 0 (sub1 (string-length part2))))))))
