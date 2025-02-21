;;; Advent of code 2024 - day18, part1 & part2 on BPI-F3 RISC-V
;;; Chez code  --- Day 14: Restroom Redoubt ---

(load "utils.so")

(define (move pp vp)
  (cons (modulo (+ (car pp) (car vp)) mxx)
	(modulo (+ (cdr pp) (cdr vp)) mxy)))

(define (move100 ppl vpl)
  (do ([xyl ppl (map move xyl vpl)]
       [n 100 (sub1 n)])
      ((zero? n) xyl)))

(define (distance x y ppl)
  (fold-left
   (lambda (d p) (+ (+ (abs (- x (car p))) (abs (- y (cdr p)))) d))
   0 ppl))

(define (run-distances ppl vpl)
  (let ((md (cons 100000 0))
	(d0 (distance (caar ppl) (cdar ppl) (cdr ppl)))
	(ppl1 (map move ppl vpl)))
    (do ([xyl ppl1 (map move xyl vpl)]
	 [n 0 (add1 n)]
	 [d d0 (distance (caar xyl) (cdar xyl) (cdr xyl))])
	((equal? xyl ppl) (cdr md))
      (when (< d (car md)) (set! md (cons d n))))))

(define (day14 file)
  (if (string=? file "input.txt")
      (begin (set! mxx 101) (set! mxy 103))
      (begin (set! mxx 11) (set! mxy 7)))
  (let* ((pvl (map (lambda (s) (string-split-n s '(#\, #\space #\p #\v #\=)))
		   (read-lines file)))
	 (pvnl (map (lambda (ss) (map string->number ss)) pvl))
	 (pv-pairs (map (lambda (ns) (list (cons (first ns) (second ns))
					   (cons (third ns) (fourth ns))))
			pvnl))
	 (ppl (map car pv-pairs))
	 (vpl (map cadr pv-pairs))
	 (grid (move100 ppl vpl))
	 (q1x (quotient mxx 2)) (q1y (quotient mxy 2))
	 (q1 (filter (lambda (p) (and (< (car p) q1x) (< (cdr p) q1y))) grid))
	 (q2 (filter (lambda (p) (and (> (car p) q1x) (< (cdr p) q1y))) grid))
	 (q3 (filter (lambda (p) (and (< (car p) q1x) (> (cdr p) q1y))) grid))
	 (q4 (filter (lambda (p) (and (> (car p) q1x) (> (cdr p) q1y))) grid)))
    (values
     (* (length q1) (length q2) (length q3) (length q4))
     (run-distances ppl vpl))))
    
