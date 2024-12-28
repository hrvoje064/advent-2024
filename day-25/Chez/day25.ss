;;; Advent of code 2024 - day25, part1, on BPI-F3 RISC-V
;;; Chez code 

(load "utils.so")

(define (get-items sl)
  (if (null? sl)
      '()
      (let-values ([(t d) (take-drop 7 sl)])
	(cons t (get-items d)))))

(define (pinh pin)
  (length  (filter (lambda (c) (char=? c #\#)) pin)))

(define (pins? lps kps)
  (for-all (lambda (l k) (< (+ k l) 6)) lps kps))

(define (search1 lock keys)
  (cond
   ((null? keys) 0)
   ((pins? (car keys) lock) (add1 (search1 lock (cdr keys))))
   (else (search1 lock (cdr keys)))))

(define (search locks keys)
  (if (null? locks)
      0
      (+ (search1 (car locks) keys)
	 (search (cdr locks) keys))))

(define (day25 file)
  (let* ((rawl (filter (lambda (s) (not (string=? s ""))) (read-lines file)))
	 (items (get-items rawl)))
    (let-values ([(locks keys)
		  (filter-split (lambda (x) (string=? (car x) "#####")) items)])
      (let* ((lockl (map (lambda (l) (cdr (map string->list l))) locks))
	     (keyl (map (lambda (l)
			  (cdr (map string->list (reverse l)))) keys))
	     (lockn
	      (map (lambda (l) (apply map (lambda xs (pinh xs)) l)) lockl))
	     (keyn
	      (map (lambda (k) (apply map (lambda xs (pinh xs)) k)) keyl)))
    (values (search lockn keyn))))))
	 

;;; memoized version
;;; =====================================================

(define (memoize3 f)
  (let ((table (make-hashtable string-hash string=?)))
    (lambda (x y z)
      (let ((seen (hashtable-ref table x #f)))
	(or seen
	    (let ((result (f x y z)))
	      (hashtable-set! table x result)
	      result))))))

(define (memoize2 f)
  (let ((table (make-hashtable equal-hash equal?)))
    (lambda (x y)
      (let ((seen (hashtable-ref table x #f)))
	(or seen
	    (let ((result (f x y)))
	      (hashtable-set! table x result)
	      result))))))

;;(set! search1 (memoize2 search1))
;;(set! search (memoize2 search))
