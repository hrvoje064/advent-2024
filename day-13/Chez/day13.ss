;;; Advent of code 2024 - day13, part1 & part2, on BPI-F3 RISC-V
;;; Chez code 

(load "utils.so")

(define (make-machine lst)
  (if (null? lst)
      '()
      (let-values ([(t d) (take-drop 3 lst)])
	(cons t (make-machine d)))))

(define (play1 ax ay bx by px py)
  (letrec
      ((rolla
	(lambda (a b maxa)
	  (cond
	   ((or (> a maxa) (< b 0)) #f)
	   ((and (= (+ (* ax a) (* bx b)) px)
		 (= (+ (* ay a) (* by b)) py))
	    (+ (* a 3) b))
	   ((or (> (+ (* ax (add1 a)) (* bx b)) px)
		(> (+ (* ay (add1 a)) (* by b)) py))
	    (rolla a (sub1 b) maxa))
	   (else (rolla (add1 a) b maxa)))))
       (rollb
	(lambda (a b maxb)
	  (cond
	   ((or (> b maxb) (< a 0)) 0)
	   ((and (= (+ (* ax a) (* bx b)) px)
		 (= (+ (* ay a) (* by b)) py))
	    (+ (* a 3) b))
	   ((or (> (+ (* ax a) (* bx (add1 b))) px)
		(> (+ (* ay a) (* by (add1 b))) py))
	    (rollb (sub1 a) b maxb))
	    (else (rollb a (add1 b) maxb))))))       
    (let ((maxa (min (quotient px ax) (quotient py ay)))
	  (maxb (min (quotient px bx) (quotient py by))))
      (if (< (* maxa 3) maxb)
	  (rollb maxa 0 (min maxb 100))
	  (rolla 0 maxb (min maxa 100))))))

(define (init-search1 clawm)
  (let* ((a (car clawm)) (b (second clawm)) (p (third clawm))
	 (ax (car a)) (ay (second a))
	 (bx (car b)) (by (second b))
	 (px (car p)) (py (second p)))
    (play1 ax ay bx by px py)))

(define (day13 file)
  (let* ((rawl (read-lines file))
	 (data
	  (filter
	   (compose not null?)
	   (map (lambda (x) 
		  (filter number? x))
		(map (lambda (l) (map string->number l))
		     (map (lambda (s)
			    (string-split-n s '(#\+ #\, #\space #\=)))
			  rawl)))))
	 (clawm (make-machine data)))
    (values
     (apply + (filter number? (map init-search1 clawm)))
     (apply + (filter number? (map init-search clawm))))))

(define (init-search clawm)
  (let* ((a (car clawm)) (b (second clawm)) (p (third clawm))
	 (ax (car a)) (ay (second a))
	 (bx (car b)) (by (second b))
	 (px (+ 10000000000000. (car p)))
	 (py (+ 10000000000000. (second p))))
    (solve ax ay bx by px py)))

(define (solve ax ay bx by px py)
  (let* ((b (quotient (- (* py ax) (* ay px)) (- (* by ax) (* bx ay))))
	 (a (quotient (- px (* b bx)) ax))
	 (a3 (inexact->exact (* a 3)))
	 (b1 (inexact->exact b)))
    (if (or (not (= (+ (* a ax) (* b bx)) px))
	    (not (= (+ (* a ay) (* b by)) py)))
	#f
	(+ a3 b))))
    
