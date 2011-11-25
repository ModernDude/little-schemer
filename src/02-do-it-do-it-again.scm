;; Is x an atom
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;; Is l a list of atoms
(define lat?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((atom? (car l)) (lat? (cdr l)))
     (else #f))))

;; Is a in the list of atoms
(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (or (equal? (car lat) a)
               (member? a (cdr lat)))))))
