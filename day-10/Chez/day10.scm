;;; Advent of code 2024 - day10, part1 & part2, on BPI-F3 RISC-V
;;; Chez code 

(load "utils.so")

(define (next grid gs th)
  (let ((i (first th)) (j (second th)) (v (third th)))
    (list (north grid gs i j v)
	  (south grid gs i j v)
	  (east grid gs i j v)
	  (west grid gs i j v))))

(define (north grid gs i j v)
  (if (= j 0)
      #f
      (let ((v1 (matrix-ref grid i (sub1 j))))
	(if (= (add1 v) v1)
	    (list i (sub1 j) v1)
	    #f))))

(define (south grid gs i j v)
  (if (= j gs)
      #f
      (let ((v1 (matrix-ref grid i (add1 j))))
	(if (= (add1 v) v1)
	    (list i (add1 j) v1)
	    #f))))

(define (east grid gs i j v)
  (if (= i gs)
      #f
      (let ((v1 (matrix-ref grid (add1 i) j)))
	(if (= (add1 v) v1)
	    (list (add1 i) j v1)
	    #f))))

(define (west grid gs i j v)
  (if (= i 0)
      #f
      (let ((v1 (matrix-ref grid (sub1 i) j)))
	(if (= (add1 v) v1)
	    (list (sub1 i) j v1)
	    #f))))

(define (trail-heads grid gs)
  (do ([i 0 (add1 i)]
       [thds '()
	     (do ([j 0 (add1 j)]
		  [row '() (let ((v (matrix-ref grid i j)))
			     (if (zero? v)
				 (cons (list i j v) row)
				 row))])
		 ((= j gs) (append row thds)))])
      ((= i gs) (reverse thds))))

(define path '())

(define (search grid gs thd)
  (let ((dl (next grid gs thd)))
    (map (lambda (th)
	   (or (and th (= (third th) 9) (set! path (cons th path)))
	       (and th
		    (map (lambda (t)
			   (when t
			     (search grid gs t)))
			 (next grid gs th)))
	       '())) dl))
  path)
       
(define (day10 file)
  (let* ((lons (map (lambda (l) (map char->number l))
		    (map string->list (read-lines file))))
	 (vecs (map list->vector lons))
	 (gs (vector-length (car vecs)))
	 (grid (list->vector vecs))
	 (thl (trail-heads grid gs)))
    (set! path '())
    (let ((result  (map (lambda (thd) (set! path '())
				(search grid (sub1 gs) thd))
			thl)))
      (values
       (apply + (map (compose length duplicates) result))
       (apply + (map length result))))))
        
    
    
	 
