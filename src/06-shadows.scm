;; Is x an atom
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;; determines if the input is an aritmetit
;; exression in infix notation
(define numbered?
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     (else
      (and (numbered? (car aexp))
           (numbered?
            (car (cdr (cdr aexp)))))))))

;; gets the first exression from an arithmetic expression triplett
(define 1st-sub-exp
  (lambda (aexp)
    (car aexp)))

;; gets the second exression from an arithmetic expresion triplett
(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

;; gets the operator part of an arithmetic exression triplett
(define operator
  (lambda (aexp)
    (car (cdr aexp))))

;; calculate the value of a an arithmetic exression
(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (operator nexp) (quote +))
      (+ (value (1st-sub-exp nexp))
         (value (2nd-sub-exp nexp))))
     ((eq? (operator nexp) (quote *))
      (* (value (1st-sub-exp nexp))
         (value (2nd-sub-exp nexp))))
     ((eq? (operator nexp) (quote -))
      (- (value (1st-sub-exp nexp))
         (value (2nd-sub-exp nexp))))
     ((eq? (operator nexp) (quote /))
      (/ (value (1st-sub-exp nexp))
         (value (2nd-sub-exp nexp)))))))

;; following 4 functions assume numbers a number is represented as
;; a list of open closed parens.
(define zzero?
  (lambda (n)
    (null? n)))

(define zadd1
  (lambda (n)
    (cons (quote ()) n)))

(define zsub1
  (lambda (n)
    (cdr n)))

(define zplus
  (lambda (n m)
    (cond
     ((zzero? m) n)
     (else (zadd1 (zplus n (zsub1 m)))))))
