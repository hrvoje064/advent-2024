;;; Advent of code 2024 - day19, part1 & part2, on BPI-F3 RISC-V
;;; Chez code 

(load "utils.so")

(define (can-match s sl twls)
  (filter
   (lambda (p) 
     (string=? (substring s 0 (min (string-length p) sl)) p)) twls))

(define (chop dsn dl)
  (lambda (pat)
    (list (substring dsn (string-length pat) dl))))

(define (tree dsnt twls)
  (let* ((dsn (car dsnt))
	 (dl (string-length dsn)))
    (if (zero? dl)
	1
	(apply + (map (lambda (dt) (tree dt twls))
		      (map (chop dsn dl) (can-match dsn dl twls)))))))

(define (prune-towels dsn twls)
  (let ((len (string-length dsn)))
    (filter (lambda (p)
	      (let f ([i 0])
		(let ((pl (string-length p)))
		  (cond
		   ((> (+ i pl) len) #f)
		   ((string=? (substring dsn i (+ i pl)) p))
		   (else (f (add1 i))))))) twls)))

(define (day19 file)
  (let* ((rawl (read-lines file))
	 (towels (sort ; greedy algorithm
		  (lambda (x y) (< (string-length x) (string-length y)))
		  (string-split-n (car rawl) '(#\space #\,))))
	 (dsns (cddr rawl))
	 (pruned (map (lambda (d) (prune-towels d towels)) dsns))
	 (result (filter (compose not zero?)
			 (map (lambda (t p) (tree (list t) p)) dsns pruned))))
    (values (length result) (apply + result))))

;;; memoized version
;;; =====================================================

(define (memoize-can-match f)
  (let ((table (make-hashtable string-hash string=?)))
    (lambda (x y z)
      (let ((seen (hashtable-ref table x #f)))
	(or seen
	    (let ((result (f x y z)))
	      (hashtable-set! table x result)
	      result))))))

(define (memoize-tree f)
  (let ((table (make-hashtable equal-hash equal?)))
    (lambda (x y)
      (let ((seen (hashtable-ref table x #f)))
	(or seen
	    (let ((result (f x y)))
	      (hashtable-set! table x result)
	      result))))))

(set! tree (memoize-tree tree))
(set! can-match (memoize-can-match can-match))
