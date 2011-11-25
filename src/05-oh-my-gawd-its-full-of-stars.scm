;; Is x an atom
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;; Add 1 to given number
(define add1
  (lambda (n)
    (+ n 1)))

;; Same as scheme's eq?
(define =?
  (lambda (n m)
    (cond
     ((gt? n m) #f)
     ((lt? n m) #f)
     (else #t))))

;; Subtract 1 from given number
(define sub1
  (lambda (n)
    (- n 1)))

;; Same as scheme's <
(define lt?
  (lambda (n m)
    (cond
     ((zero? m) #f)
     ((zero? n) #t)
     (else (lt? (sub1 n) (sub1 m))))))

;; Same as scheme's >
(define gt?
  (lambda (n m)
    (cond
     ((zero? n) #f)
     ((zero? m) #t)
     (else (gt? (sub1 n) (sub1 m))))))

;; Equal if the same atom
(define eqan?
  (lambda (a1 a2)
    (cond
     ((and (number? a1) (number? a2))
      (=? a1 a2))
     ((or (number? a1) (number? a2))
      #f)
     (else (eq? a1 a2)))))

;; remove atom from s-expression
(define rember*
  (lambda (a l)
    (cond
     ((null? l) (quote ()))
     ((atom? (car l))
      (cond
       ((eq? (car l) a)
        (rember* a (cdr l)))
       (else (cons (car l)
                   (rember* a (cdr l))))))
     (else (cons (rember* a (car l))
                 (rember* a (cdr l)))))))

;; insert the atom new to the right of the atom old anywhere in the
;; sexp
(define insertR*
  (lambda (new old l)
    (cond
     ((null? l) (quote ()))
     ((atom? (car l))
      (cond
       ((eq? (car l) old)
        (cons old
              (cons new
                    (insertR* new old
                              (cdr l)))))
       (else (cons (car l)
                   (insertR* new old
                             (cdr l))))))
     (else (cons (insertR* new old
                           (car l))
                 (insertR* new old
                           (cdr l)))))))

;; count the number of times that the atom a is found in the given
;; list
(define occur*
  (lambda (a l)
    (cond
     ((null? l) 0)
     ((atom? (car l))
      (cond
       ((eq? (car l) a)
        (add1 (occur* a (cdr l))))
       (else (occur* a (cdr l)))))
     (else (+ (occur* a (car l))
              (occur* a (cdr l)))))))

;; substitute new for old anywhere in the sexp l
(define subst*
  (lambda (new old l)
    (cond
     ((null? l) (quote ()))
     ((atom? (car l))
      (cond
       ((eq? (car l) old)
        (cons new
              (subst* new old (cdr l))))
       (else (cons (car l)
                   (subst* new old
                           (cdr l))))))
     (else
      (cons (subst* new old (car l))
            (subst* new old (cdr l)))))))

(define insertL*
  (lambda (new old l)
    (cond
     ((null? l) (quote ()))
     ((atom? (car l))
      (cond
       ((eq? (car l) old)
        (cons new
              (cons old
                    (insertL* new old
                              (cdr l)))))
       (else (cons (car l)
                   (insertL* new old
                             (cdr l))))))
     (else (cons (insertL* new old
                           (car l))
                 (insertL* new old
                           (cdr l)))))))

;; determines if a is a member of the sexp l
(define member*
  (lambda (a l)
    (cond
     ((null? l) #f)
     ((atom? (car l))
      (or (eq? (car l) a)
          (member* a (cdr l))))
     (else (or (member* a (car l))
               (member* a (cdr l)))))))

;; find leftmost atom in list
(define leftmost
  (lambda (l)
    (cond
     ((atom? (car l)) (car l))
     (else (leftmost (car l))))))

;; same number, atom or list
(define _equal?
  (lambda (s1 s2)
    (cond
     ((and (atom? s1) (atom? s2))
      (eqan? s1 s2))
     ((or (atom? s1) (atom? s2))
      #f)
     (else (eqlist? s1 s2)))))

;; tests if two lists are equal
(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((or (null? l1) (null? l2)) #f)
     (else
      (and (_equal? (car l1) (car l2))
           (eqlist? (cdr l1) (cdr l2)))))))
