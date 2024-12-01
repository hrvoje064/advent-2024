
;;; File operations in Chez
;;; ==========================================================

(module Utils
  (time time-void take-drop take drop compose
	read-file parse-file-lst parse-lst-strs read-lines
	read-lol-words string-split file->string)

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
)

(import Utils)
