;; Is x an atom
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

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

;; remove a from l for all instances
;; in whine test? is true for a and the
;; current time in l
(define rember-f
  (lambda (test? a l)
    (cond
     ((null? l) (quote ()))
     ((test? (car l) a) (cdr l))
     (else (cons (car l)
                 (rember-f test? a
                           (cdr l)))))))

;;create a funcion testing if val is eq? to a
(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

;; func that returns true if val is eq? salad
(define eq?-salad
  (eq?-c 'salad))

;; build a funtion that takes a predicate that determines
;; if if a given item should be removed from the list
(define rember-ff
  (lambda (test?)
    (lambda (a l)
      (cond
       ((null? l) (quote ()))
       ((test? (car l) a) (cdr l))
       (else (cons (car l)
                   ((rember-ff test?) a
                    (cdr l))))))))

;; build a funtion that removes from lat using eq? funtion
(define rember-eq? (rember-ff eq?))

;; build a function that insterts to the left using the
;; given predicate
(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
       ((null? l) (quote ()))
       ((test? (car l) old)
        (cons new (cons old (cdr l))))
       (else (cons (car l)
                   ((insertL-f test?) new old
                    (cdr l))))))))
;; build a function that insersts to the right using the given
;; predicate
(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
       ((null? l) (quote ()))
       ((test? (car l) old)
        (cons old (cons new (cdr l))))
       (else (cons (car l)
                   ((insertR-f test?) new old
                    (cdr l))))))))

(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
       ((null? l) (quote ()))
       ((eq? (car l) old)
        (seq new old (cdr l)))
       (else (cons (car l)
                   ((insert-g seq) new old
                    (cdr l))))))))

;; insert to left using seq abstraction
(define insert-g-L (insert-g seqL))

(define insert-g-R (insert-g seqR))

;; same as above but better because we don's need
;; to define seqL
(define insert-g-LL (insert-g
                     (lambda (new old l)
                       (cons new (cons old l)))))

(define insert-g-RR (insert-g
                     (lambda (new old l)
                       (cons old (cons new l)))))

;; swap new with old. cool because we have abstraction the
(define subst-g (insert-g
                 (lambda (new old l)
                   (cons new l))))

;; rtffd
(define atom-to-function
  (lambda (x)
    (cond
     ((eq? x (quote +)) +)
     ((eq? x (quote *)) *)
     (else expt))))

;; calc value of aritmetic exrpression
(define avalue
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     (else
      ((atom-to-function
        (operator nexp))
       (avalue (1st-sub-exp nexp))
       (avalue (2nd-sub-exp nexp)))))))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
       ((null? lat) (quote ()))
       ((test? a (car lat))
        ((multirember-f test?) a
         (cdr lat)))
       (else (cons (car lat)
                   ((multirember-f test?) a
                    (cdr lat))))))))

;; creates a funtion that returns true if the input is
;; eq? tuna
(define eq?-tuna
  (eq?-c (quote tuna)))

;; combine a and test
(define multiremberT
  (lambda (test? lat)
    (cond
     ((null? lat) (quote ()))
     ((test? (car lat))
      (multiremberT test? (cdr lat)))
     (else (cons (car lat)
                 (multiremberT test? (cdr lat)))))))

;; our first continuation!!!
;; col is a collector/continuation
;;
;; this function looks at every atom in lat and checks to
;; see if it is eq? to a. The atoms that are not eq? are
;; collected into ls1. The ones that match are put into
;; ls2. Then the following is called (a-friend ls1 ls2)
(define multirember&co
  (lambda (a lat col)
    (cond
     ((null? lat)
      (col (quote ()) (quote ())))
     ((eq? (car lat) a)
      (multirember&co a
                      (cdr lat)
                      (lambda (newlat seen)
                        (col newlat
                             (cons (car lat) seen)))))
     (else
      (multirember&co a
                      (cdr lat)
                      (lambda (newlat seen)
                        (col (cons (car lat) newlat)
                             seen)))))))
;; inital collector/continuation
(define a-friend
  (lambda (x y)
    (null? y)))

;; insert new to the left and old  to the right
(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? (car lat) oldL)
      (cons new
            (cons oldL
                  (multiinsertLR new oldL oldR
                                 (cdr lat)))))
     ((eq? (car lat) oldR)
      (cons oldR
            (cons new
                  (multiinsertLR new oldL oldR (cdr lat)))))
     (else
      (cons (car lat)
            (multiinsertLR new oldL oldR
                           (cdr lat)))))))

;; insert to left and right ond store in collector
;; the updated list as well as counts of insersts L and R
(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
     ((null? lat)
      (col (quote ()) 0 0))
     ((eq? (car lat) oldL)
      (multiinsertLR&co new oldL oldR
                        (cdr lat)
                        (lambda (newlat L R)
                          (col (cons new
                                     (cons oldL newlat))
                               (add1 L) R))))
     ((eq? (car lat) oldR)
      (multiinsertLR&co new oldL oldR
                        (cdr lat)
                        (lambda (newlat L R)
                          (col (cons oldR (cons new newlat))
                               L (add1 R)))))
     (else
      (multiinsertLR&co new oldL oldR
                        (cdr lat)
                        (lambda (newlat L R)
                          (col (cons (car lat) newlat)
                               L R)))))))

(define multiinsertLR&co-total-changes
  (lambda (newlat L R)
    (+ L R)))

;; diff from even def in book
(define even?
  (lambda (x)
    (= (remainder x 2) 0)))

;; remove all odd numbers from a list of nested lists
(define evens-only*
  (lambda (l)
    (cond ((null? l) (quote ()))
          ((atom? (car l))
           (cond ((even? (car l))
                  (cons (car l)
                        (evens-only* (cdr l))))
                 (else (evens-only* (cdr l)))))
          (else (cons (evens-only* (car l))
                      (evens-only*  (cdr l)))))))

;;;build a nested list of even numbers. Get a result that is
;; the product of all even numbers. Get a number that is a nom of all of
;; all odd numbers.
(define evens-only*&co
  (lambda (l col)
    (cond ((null? l) (col '() 1 0))
          ((atom? (car l))
           (cond ((even? (car l))
                  (evens-only*&co
                   (cdr l)
                   (lambda (newl p s)
                     (col (cons (car l) newl)
                          (* (car l) p)
                          s))))
                 (else (evens-only*&co
                        (cdr l)
                        (lambda (newl p s)
                          (col newl p (+ s (car l))))))))
          (else (evens-only*&co
                 (car l)
                 (lambda (al ap as)
                   (evens-only*&co (cdr l)
                                   (lambda (dl dp ds)
                                     (col (cons al dl)
                                          (* ap dp)
                                          (+ as ds))))))))))

(define the-last-friend
  (lambda (newl product sum)
    (cons sum
          (cons product
                newl))))
