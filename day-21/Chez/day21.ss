;;; Advent of code 2024 - day21, part1 & part2, on BPI-F3 RISC-V
;;; Chez code  --- Day 21: Keypad Conundrum ---

(load "utils.so")

(define movest (make-hashtable equal-hash equal? 64))
(define xpandt (make-hashtable equal-hash equal? 64))

(define dkplt
  '(((#\A . #\^) (#\<  #\A)) ((#\^ . #\A) (#\> #\A))
    ((#\A . #\>) (#\v #\A)) ((#\> . #\A) (#\^ #\A))
    ((#\A . #\v) (#\< #\v #\A)) ((#\v . #\A) (#\^ #\> #\A))
    ((#\A . #\<) (#\v #\< #\< #\A)) ((#\< . #\A) (#\> #\> #\^ #\A))
    ((#\^ . #\v) (#\v #\A)) ((#\v . #\^) (#\^ #\A))
    ((#\^ . #\>) (#\v #\> #\A)) ((#\> . #\^) (#\< #\^ #\A))
    ((#\^ . #\<) (#\v #\< #\A)) ((#\< . #\^) (#\> #\^ #\A))
    ((#\v . #\>) (#\> #\A)) ((#\> . #\v) (#\< #\A))
    ((#\v . #\<) (#\< #\A)) ((#\< . #\v) (#\> #\A))
    ((#\> . #\<) (#\< #\< #\A)) ((#\< . #\>) (#\> #\> #\A))
    ((#\A . #\A) (#\A)) ((#\^ . #\^) (#\A)) ((#\> . #\>) (#\A))
    ((#\v . #\v) (#\A)) ((#\< . #\<) (#\A))))

(define nkplt
  '(((#\A . #\8) (#\< #\^ #\^ #\^ #\A)) ((#\8 . #\0) (#\v #\v #\v #\A))
    ((#\0 . #\5) (#\^ #\^ #\A)) ((#\5 . #\A) (#\v #\v #\> #\A))
    ((#\A . #\9) (#\^ #\^ #\^ #\A)) ((#\9 . #\6) (#\v #\A))
    ((#\6 . #\4) (#\< #\< #\A)) ((#\4 . #\A) (#\> #\> #\v #\v #\A))
    ((#\A . #\4) (#\^ #\^ #\< #\< #\A)) ((#\4 . #\5) (#\> #\A))
    ((#\5 . #\9) (#\^ #\> #\A)) ((#\9 . #\A) (#\v #\v #\v #\A))
    ((#\6 . #\8) (#\< #\^ #\A)) ((#\8 . #\A) (#\v #\v #\v #\> #\A))
    ((#\A . #\6) (#\^ #\^ #\A)) ((#\6 . #\7) (#\< #\< #\^ #\A))
    ((#\7 . #\1) (#\v #\v #\A)) ((#\1 . #\A) (#\> #\> #\v #\A))
    ((#\A . #\0) (#\< #\A)) ((#\0 . #\2) (#\^ #\A))
    ((#\2 . #\9) (#\> #\^ #\^ #\A))
    ((#\9 . #\8) (#\< #\A)) ((#\8 . #\0) (#\v #\v #\v #\A))
    ((#\0 . #\A) (#\> #\A))
    ((#\A . #\1) (#\^ #\< #\< #\A)) ((#\1 . #\7) (#\^ #\^ #\A))
    ((#\7 . #\9) (#\> #\> #\A))
    ((#\5 . #\6) (#\> #\A)) ((#\6 . #\A) (#\v #\v #\A))
    ((#\A . #\3) (#\^ #\A)) ((#\3 . #\7) (#\< #\< #\^ #\^ #\A))))

(define (list->movest lst)
  (if (null? lst)
      'done
      (begin
	(hashtable-set! movest (caar lst) (cadar lst))
	(list->movest (cdr lst)))))

(define (enter-code from code)
  (if (null? code)
      '()
      (append (hashtable-ref movest (cons from (car code)) #f)
	      (enter-code (car code) (cdr code)))))

(define (ndcode from)
  (lambda (code) (enter-code from code)))

(define (directions acc gens from code)
  (if (zero? gens)
      acc
      (let* ((new-code (enter-code from code)))	     
	(directions (length new-code) (sub1 gens) #\A new-code))))

(define (expand from to gens)
  (directions 0 gens from (list to)))

(define (part2 generations starting-code)
  (map
   (lambda (code)
     (fold-left
      (lambda (r ch)
	(let ((new-r (expand (cadr r) ch generations)))
	  (list (+ (car r) new-r) ch))) '(0 #\A) code)) starting-code))

(define (day21 file)
  (list->movest (append dkplt nkplt))
  (hashtable-clear! xpandt)
  (let* ((codes (read-lines file))
	 (numbers
	  (map (lambda (c)
		 (string->number (substring c 0 3))) codes))
	 (codel (map string->list codes))
	 (d2l (map (repeat (ndcode #\A) 2) codel))
	 (d11l (map (repeat (ndcode #\A) 11) codel))
	 (d25x (part2 14 d11l)))
	(values
	 (apply + (map * (map length d2l) numbers))
	 (apply + (map * (map car d25x) numbers)))))
    
;;; memoize 
;;; ================================================================

(define (memoize-expand f)
  (lambda (x y z)
    (let ((seen (hashtable-ref xpandt (cons x y) #f)))
      (or seen
          (let ((result (f x y z)))
            (hashtable-set! xpandt (cons x y) result)
            result)))))
   
(set! expand (memoize-expand expand))
