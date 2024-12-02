
;;; File operations in Chez
;;; ==========================================================

(module Utils
  (time time-void take-drop take drop compose
	read-file parse-file-lst parse-lst-strs read-lines
	read-lol-words string-split list-split file->string
	make-bst bst-histogram bst-get)

  (define (time thunk)
    (collect)
    (let* ((start (current-time))
	   (result (thunk))
	   (end (current-time)))
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
    
  )

(import Utils)
