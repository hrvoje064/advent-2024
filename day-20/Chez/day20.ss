


;;; Advent of code 2024 - day20, part1 & part2, on BPI-F3 RISC-V
;;; Chez code  --- Day 20: Race Condition ---

(load "utils.so")

(define grid #f) ; grid matrix
(define path-ht (make-hashtable equal-hash equal? 9484))
(define mxlimit #f)
(define path #f)
(define min-cut 100)
(define max-cut 2)

(define (init-grid rows)
  (do ([m (make-vector mxlimit)]
       [i 0 (add1 i)]
       [c rows (cdr c)])
      ((= i mxlimit) m)
    (vector-set!
     m i (list->vector
	  (map (lambda (ch) (if (char=? ch #\#) #f ch)) (car c))))))

(define (findSE s/e)
  (let ((point #f))
    (do ([j 0 (add1 j)])
	((= j mxlimit) point)
      (do ([i 0 (add1 i)])
	  ((= i mxlimit))
	(let ((ch (matrix-ref grid j i)))
	(when (and ch (char=? s/e ch))
	  (set! point (cons j i))))))))

(define (init-path path len)
  (do ([p path (cdr p)]
       [i 0 (add1 i)])
      ((null? p))
    (hashtable-set! path-ht (car p) (- len i))))

(define (neighbors from)
  (let
      ((east (let ((x (add1 (car from))) (y (cdr from)))
	       (and (< x mxlimit) (matrix-ref grid x y) (cons x y))))
       (west (let ((x (sub1 (car from))) (y (cdr from)))
	       (and (>= x 0) (matrix-ref grid x y) (cons x y))))
       (south (let ((x (car from)) (y (add1 (cdr from))))
		(and (< y mxlimit) (matrix-ref grid x y) (cons x y))))
       (north (let ((x (car from)) (y (sub1 (cdr from))))
		(and (>= y 0) (matrix-ref grid x y) (cons x y)))))
    (filter values (list north east south west))))

(define (run seen from to)
  (cond
   ((equal? from to) (set! path (reverse seen)) 0)
   (else
    (let ((nbrs (neighbors from)))
      (let-values ([(not-seen new-seen) (check-seen nbrs seen)])
	(add1 (run new-seen (car not-seen) to)))))))

(define (check-seen new old)
  (cond
   ((null? new) (values '() old))
   (else
    (let-values ([(n o) (check-seen (cdr new) old)])
      (if (member (car new) old)
	  (values n o)
	  (values (cons (car new) n) (cons (car new) o)))))))

(define (search from to n)
  (let ((dx (abs (- (car from) (car to))))
	(dy (abs (- (cdr from) (cdr to)))))
    (+ dx dy)))

(define (legal0 x)
  (or (and (>= x 0) x) 0))

(define (legal20 x)
  (or (and (< x mxlimit) x) (sub1 mxlimit)))

(define (in20steps pos)
  (let* ((x1 (car pos)) (y1 (cdr pos))
	 (maxj (legal20 (+ x1 max-cut)))
	 (maxi (legal20 (+ y1 max-cut)))
	 (cuts '()))
    (do ([j (legal0 (- x1 max-cut)) (add1 j)])
	((> j maxj) cuts)
      (do ([i (legal0 (- y1 max-cut)) (add1 i)])
	  ((> i maxi))
	(when (and (<= (+ (abs (- x1 j)) (abs (- y1 i))) max-cut)
		   (matrix-ref grid j i))
	  (set! cuts (cons (cons j i) cuts)))))))

(define (prune-cuts pos cuts)
  (let ((d1 (hashtable-ref path-ht pos #f)))
    (filter (lambda (p) (> (- d1 (hashtable-ref path-ht p #f)) min-cut))
	    cuts)))

(define (measure-cuts pos cuts)
  (let ((d1 (hashtable-ref path-ht pos #f)))
    (filter
     (compose not zero?)
     (map
      (lambda (c)
	(let ((d (- d1 (+ (search c pos 0) (hashtable-ref path-ht c #f)))))
	  (or (and (>= d min-cut) d) 0))) cuts))))

(define (get-cuts pos)
  (measure-cuts pos (prune-cuts pos (in20steps pos))))

(define (day20 file)
  (hashtable-clear! path-ht)
  (let ((rows (map string->list (read-lines file))))
    (set! mxlimit (length (car rows)))
    (set! grid (init-grid rows))
    (let* ((S (findSE #\S)) (E (findSE #\E))
	   (r (run (list S) S E)))
      (init-path path r)
      (set! max-cut 2)
      (let ((part1 (length (flatten (map get-cuts (take (- r min-cut) path))))))
	(set! max-cut 20)
	(let ((part2 (length (flatten (map get-cuts (take (- r min-cut) path))))))
	  (values part1 part2))))))
