;;; fib memoize tests

(load "utils.so")

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (memoize f)
  (let ((table (make-eq-hashtable)))
    (lambda (x)
      (let ((seen (hashtable-ref table  x #f)))
        (or seen
            (let ((result (f x)))
              (hashtable-set! table x result)
              result))))))

(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))
