
;;; File operations in Chez
;;; ==========================================================
;;; to compile: echo '(compile-file "utils.ss")' | scheme -q

;; (module Utils
;;   (time time-void take-drop take drop compose
;; 	read-file parse-file-lst parse-lst-strs read-lines
;; 	read-lol-words string-split list-split file->string
;; 	make-bst bst-histogram bst-get string-split-parse time2
;; 	first second third fourth fifth sixth seventh eight)

(module Utils

  (time time-void take-drop take drop compose
	read-file parse-file-lst parse-lst-strs read-lines
	read-lol-words string-split string-split-n list-split file->string
	make-bst bst-histogram bst-get string-split-parse time2
 	first second third fourth fifth sixth seventh eight string-split-c
	make-matrix matrix? matrix-rows matrix-columns matrix-ref matrix-set!
	mul)

  (define (time2 thunk)
    (collect)
    (let*-values ([(start) (current-time)]
		  [(part1 part2) (thunk)]
		  [(end) (current-time)])
      (values part1 part2 (time-difference end start))))

   (define (time thunk)
    (collect)
    (let* ((start (current-time))
	   (result (thunk))
	   (end  (current-time)))
      (values result (time-difference end start))))
 
  (define (time-void thunk)
    (collect)
    (let* ((start (current-time))
	   (result (thunk))
	   (end (current-time)))
      (values (time-difference end start))))

  (define (take-drop n l)
    (if (or (zero? n) (null? l))
	(values '() l)
	(let-values ([(t d) (take-drop (sub1 n) (cdr l))])
	  (values (cons (car l) t) d))))

  (define (take n l)
    (let-values ([(t _) (take-drop n l)])
      t))

  (define (drop n l)
    (let-values ([(_ d) (take-drop n l)])
      d))

  (define (compose f g)
    (lambda (x) (f (g x))))

  (define (compose-n . fs) ; slow
    (lambda (x)
      (if (null? fs)
	  x
	  ((car fs) ((apply compose-n (cdr fs)) x)))))

  (define (list-split lst)
    (if (null? lst)
	(values '() '())
	(let-values ([(l r) (list-split (cddr lst))])
	  (values (cons (car lst) l) (cons (cadr lst) r)))))

  (define first car)
  (define second cadr)
  (define third caddr)
  (define fourth cadddr)
  (define fifth (compose car cddddr))
  (define sixth (compose cadr cddddr))
  (define seventh (compose caddr cddddr))
  (define eight (compose cadddr cddddr))

;;; ==========================================================

  ;; read a file into a list of objects (numbers, symbols,strings ...)
  (define (read-file file)
    (let ((p (open-input-file file)))
      (let f ((x (read p)))
	(if (eof-object? x)
	    (begin
	      (close-port p)
	      '())
	    (cons x (f (read p)))))))

  ;; parse objects to lists of numbers and strings
  (define (parse-file-lst l n)
    (if (null? l)
	'()
	(let-values ([(t d) (take-drop n l)])
	  (cons
	   (map (lambda (x)
		  (cond
		   ((symbol? x) (symbol->string x))
		   (else x))) t)
	   (parse-file-lst d n)))))

  ;; parse line strings to lists of numbers and strings
  (define (parse-lst-strs lst)
    (map (lambda (ll)
	   (map (lambda (s) (let ((n (string->number s))) (if n n s))) ll))
	 (map string-split-space lst)))

  ;; read a file into a list of strings
  (define (read-lines file)
    (let ((p (open-input-file file)))
      (let f ((x (get-line p)))
	(if (eof-object? x)
	    (begin
	      (close-port p)
	      '())
	    (cons x (f (get-line p)))))))

  ;; read a file into list of lists of strings
  ;; splits every string (from read-lines) into a list of
  ;; word strings split on space.
  (define (read-lol-words file)
    (let ((p (open-input-file file)))
      (let f ((x (get-line p)))
	(if (eof-object? x)
	    (begin
	      (close-port p)
	      '())
	    (cons (string-split x) (f (get-line p)))))))

  (define string-split
    (case-lambda
      [(s) (string-split-c s #\space)]
      [(s c) (if (char? c) (string-split-c s c) (string-split-n s c))]))
  
  ;; split string s on character c
  (define (string-split-c s c)
    (let ((len (string-length s)))
      (let split ((a 0) (b 0))
	(cond
	 ((= b len) (if (= a b) '() (list (substring s a b))))
	 ((char=? (string-ref s b) c)
	  (if (= a b)
	      (split (add1 a) (add1 b))
	      (cons (substring s a b) (split (add1 b) (add1 b)))))
	 (else (split a (add1 b)))))))

  ;; split string s on list of characters
  (define (string-split-n s cl)
    (let ((len (string-length s)))
      (let split ((a 0) (b 0))
	(cond
	 ((= b len) (if (= a b) '() (list (substring s a b))))
	 ((memq (string-ref s b) cl)
	  (if (= a b)
	      (split (add1 a) (add1 b))
	      (cons (substring s a b) (split (add1 b) (add1 b)))))
	 (else (split a (add1 b)))))))

  ;; split string s on list of characters - inserting actual character
  (define (string-split-parse s cl)
    (let ((len (string-length s)))
      (let split ((a 0) (b 0))
	(if (= b len)
	    (if (= a b) '() (list (substring s a b)))
	    (let ((c (memq (string-ref s b) cl)))
	      (cond
	       (c (if (= a b)
		      (split (add1 a) (add1 b))
		      (cons (substring s a b)
			    (cons (string (car c))
				  (split (add1 b) (add1 b))))))
	       (else (split a (add1 b)))))))))
  
  ;; whole file in one large string - very fast
  (define (file->string file)
    (apply string-append
	   (map (lambda (x) (string-append " " x)) (read-lines file))))

  ;; inserting elements into a BST
  ;; BST type: '((k . v) lbst rbst) =
  ;; ((caar . cdar) cadr caddr)  
  (define (add f i x bst)
    (cond
     ((null? bst) (list (cons x (f i)) '() '()))
     ((= (caar bst) x) (cons (cons x (f (cdar bst))) (cdr bst)))
     ((< x (caar bst)) (list (car bst) (add f i x (cadr bst)) (caddr bst)))
     (else (list (car bst) (cadr bst) (add f i x (caddr bst))))))

  ;; creating a fully ballanced BST from list
  ;; BST type: '((k . v) lbst rbst)
  (define (list->bst f i bst l n)
    (if (null? l)
	bst
	(let-values ([(a b) (take-drop n l)])
	  (list->bst
	   f i (list->bst f i (add f i (car b) bst) (cdr b) (quotient (sub1 n) 2))
	   a (quotient n 2)))))

  (define (make-bst f i l)
    (list->bst f i '() (sort < l) (quotient (length l) 2)))

  ;; BST as histogram
  (define (addh x bst)
    (cond
     ((null? bst) (list (cons x 1) '() '()))
     ((= (caar bst) x) (cons (cons x (add1 (cdar bst))) (cdr bst)))
     ((< x (caar bst)) (list (car bst) (addh x (cadr bst)) (caddr bst)))
     (else (list (car bst) (cadr bst) (addh x (caddr bst))))))

  (define (list->bsth bst l n)
    (if (null? l)
	bst
	(let-values ([(a b) (take-drop n l)])
	  (list->bsth
	   (list->bsth (addh (car b) bst) (cdr b) (quotient (sub1 n) 2))
	   a (quotient n 2)))))

  (define (bst-histogram lst) ;; tl4 4.8s , tl3 13.9s
    (list->bsth '() (sort < lst) (quotient (length lst) 2)))

  (define (bst-get i x bst)
    (cond
     ((null? bst) i)
     ((= (caar bst) x) (cdar bst))
     ((> (caar bst) x) (bst-get i x (cadr bst)))
     (else (bst-get i x (caddr bst)))))

  ;; make-matrix creates a matrix (a vector of vectors).
  (define make-matrix
    (lambda (rows columns)
      (do ([m (make-vector rows)]
           [i 0 (+ i 1)])
          ((= i rows) m)
        (vector-set! m i (make-vector columns)))))

  ;; matrix? checks to see if its argument is a matrix.
  ;; It isn't foolproof, but it's generally good enough.
  (define matrix?
    (lambda (x)
      (and (vector? x)
           (> (vector-length x) 0)
           (vector? (vector-ref x 0)))))

  ;; matrix-rows returns the number of rows in a matrix.
  (define matrix-rows
    (lambda (x)
      (vector-length x)))

  ;; matrix-columns returns the number of columns in a matrix.
  (define matrix-columns
    (lambda (x)
      (vector-length (vector-ref x 0))))

  ;; matrix-ref returns the jth element of the ith row.
  (define matrix-ref
    (lambda (m i j)
      (vector-ref (vector-ref m i) j)))

  ;; matrix-set! changes the jth element of the ith row.
  (define matrix-set!
    (lambda (m i j x)
      (vector-set! (vector-ref m i) j x)))

  ;; mat-sca-mul multiplies a matrix by a scalar.
  (define mat-sca-mul
    (lambda (m x)
      (let* ([nr (matrix-rows m)]
             [nc (matrix-columns m)]
             [r (make-matrix nr nc)])
        (do ([i 0 (+ i 1)])
            ((= i nr) r)
          (do ([j 0 (+ j 1)])
              ((= j nc))
            (matrix-set! r i j (* x (matrix-ref m i j))))))))

  ;; mat-mat-mul multiplies one matrix by another, after verifying
  ;; that the first matrix has as many columns as the second
  ;; matrix has rows.
  (define mat-mat-mul
    (lambda (m1 m2)
      (let* ([nr1 (matrix-rows m1)]
             [nr2 (matrix-rows m2)]
             [nc2 (matrix-columns m2)]
             [r (make-matrix nr1 nc2)])
        (unless (= (matrix-columns m1) nr2) (match-error m1 m2))
        (do ([i 0 (+ i 1)])
            ((= i nr1) r)
          (do ([j 0 (+ j 1)])
              ((= j nc2))
            (do ([k 0 (+ k 1)]
                 [a 0 (+ a
                         (* (matrix-ref m1 i k)
                            (matrix-ref m2 k j)))])
                ((= k nr2)
                 (matrix-set! r i j a))))))))

  ;; type-error is called to complain when mul receives an invalid
  ;; type of argument.
  (define type-error
    (lambda (what)
      (assertion-violation 'mul
			   "not a number or matrix"
			   what)))

  ;; match-error is called to complain when mul receives a pair of
  ;; incompatible arguments.
  (define match-error
    (lambda (what1 what2)
      (assertion-violation 'mul
			   "incompatible operands" what1
			   what2)))
  
  ;; mul is the generic matrix/scalar multiplication procedure
  (define mul
    (lambda (x y)
      (cond
       [(number? x)
        (cond
         [(number? y) (* x y)]
         [(matrix? y) (mat-sca-mul y x)]
         [else (type-error y)])]
       [(matrix? x)
        (cond
         [(number? y) (mat-sca-mul x y)]
         [(matrix? y) (mat-mat-mul x y)]
         [else (type-error y)])]
       [else (type-error x)])))

  )

(import Utils)
