;; Is x an atom
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;; Take a list and build another list composed of the first sexp
;; within each internal sublist
(define firsts
  (lambda (l)
    (cond
     ((null? l) '())
     (else (cons (car (car l))
                 (firsts (cdr l)))))))

;; Is a in the list of atoms
(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (or (equal? (car lat) a)
               (member? a (cdr lat)))))))

;; is the lat a set
(define set?
  (lambda (lat)
    (cond
     ((null? lat) #t)
     ((member? (car lat) (cdr lat)) #f)
     (else (set? (cdr lat))))))

;; make a set from the lat
(define makeset
  (lambda (lat)
    (cond
     ((null? lat) (quote ()))
     (else (cons (car lat)
                 (makeset
                  (multirember (car lat)
                               (cdr lat))))))))

;; is set1 a subset of set2
(define subset?
  (lambda (set1 set2)
    (cond
     ((null? set1) #t)
     (else
      (and (member? (car set1) set2)
           (subset? (cdr set1) set2))))))

;; are sets equal
(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
         (subset? set2 set1))))

(define intersect
  (lambda (set1 set2)
    (cond
     ((null? set1) (quote ()))
     ((member? (car set1) set2)
      (cons (car set1)
            (intersect (cdr set1) set2)))
     (else (intersect (cdr set1) set2)))))

(define union
  (lambda (set1 set2)
    (cond
     ((null? set1) set2)
     ((member? (car set1) set2)
      (union (cdr set1) set2))
     (else (cons (car set1)
                 (union (cdr set1) set2))))))

(define intersectall
  (lambda (l-set)
    (cond
     ((null? (cdr l-set)) (car l-set))
     (else (intersect (car l-set)
                      (intersectall (cdr l-set)))))))

;; is x a pair!!!!!!!!!
(define a-pair?
  (lambda (x)
    (cond
     ((atom? x) #f)
     ((null? x) #f)
     ((null? (cdr x)) #f)
     ((null? (cdr (cdr x))) #t)
     (else #f))))

;; get the first time from a pair
(define first
  (lambda (p)
    (car p)))

;; get the second time from a pair
(define second
  (lambda (p)
    (car (cdr p))))

;; make a pair
(define build
  (lambda (s1 s2)
    (cons s1
          (cons s2 (quote ())))))

;;rel is a set of pairs

;; is rel a "finite function" -> a list of pairs in
;; which no first element of any pair is the same as any
;; other first element
(define fun?
  (lambda (rel)
    (set? (firsts rel))))

;; reverse one pair
(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

;; reverse each pair in the relation
(define revrel
  (lambda (rel)
    (cond
     ((null? rel) (quote ()))
     (else (cons (revpair (car rel))
                 (revrel (cdr rel)))))))

;; get all soc
(define seconds
  (lambda (l)
    (cond
     ((null? l) '())
     (else (cons (car (cdr (car l)))
                 (seconds (cdr l)))))))

;; second of each pair must make a set
(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))

;; same as full fun
(define one-to-one?
  (lambda (fun)
    (fun? (revrel fun))))

