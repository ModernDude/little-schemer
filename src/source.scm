

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
     (else (or (eq? (car lat) a)
               (member? a (cdr lat)))))))


;; Take an atom and a list of atoms and makes a new list of atoms with
;; the first occurence of the atom in the old list removed.
(define rember
  (lambda (a lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? (car lat) a) (cdr lat))
     (else (cons (car lat)
                 (rember a (cdr lat)))))))


;; Take a list and build another list composed of the first sexp
;; within each internal sublist
(define firsts
  (lambda (l)
    (cond
     ((null? l) '())
     (else (cons (car (car l))
                 (firsts (cdr l)))))))
     
;; Build a lat with new inserted to the right of the first occurance
;; of old.
(define insertR
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     (else (cond
            ((eq? (car lat) old)
             (cons old
                   (cons new (cdr lat))))
             (else (cons (car lat)
                   (insertR new old
                            (cdr lat)))))))))

;; Build a lat with new inserted to the left  of the first occurance of old.
(define insertL
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     (else (cond
            ((eq? (car lat) old)
             (cons new lat))
            (else (cons (car lat)
                        (insertL new old
                                 (cdr lat)))))))))

;; Replaces the first occurrence of old in the lat with new.
(define subst
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     (else (cond
            ((eq? (car lat) old)
             (cons new (cdr lat)))
            (else (cons (car lat)
                        (subst new old
                               (cdr lat)))))))))

;; Replaces the first occurrence of o1 or the first occurrence of o2 by new 
(define subst2
  (lambda (new o1 o2 lat)
    (cond
     ((null? lat) (quote ()))
     (else (cond
            ((or (eq? (car lat) o1)
                 (eq? (car lat) o2))
                (cons new (cdr lat)))
            (else (cons (car lat)
                        (subst2 new o1 o2 (cdr lat)))))))))

;; Remove all occurrences of the a from lat
(define multirember
  (lambda (a lat)
    (cond
     ((null? lat) (quote ()))
     (else
      (cond
       ((eq? (car lat) a)
        (multirember a (cdr lat)))
       (else (cons (car lat)
                   (multirember a  (cdr lat)))))))))

;; Insert new to the right of each occurrence of old
(define multiinsertR
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     (else
      (cond
       ((eq? (car lat) old) 
        (cons (car lat)
              (cons new
                    (multiinsertR new old
                                  (cdr lat)))))
       (else (cons (car lat)
                   (multiinsertR new old (cdr lat)))))))))

;; Insert new to the left of each occurrence of old
(define multiinsertL
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     (else
      (cond
       ((eq? (car lat) old)
        (cons new
              (cons old
                    (multiinsertL new old
                                  (cdr lat)))))
       (else (cons (car lat)
                   (multiinsertL new old (cdr lat)))))))))


;; Replace all occurrences of old with new within lat.
(define multisubst
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     (else (cond
            ((eq? (car lat) old)
             (cons new
                   (multisubst new old
                               (cdr lat))))
            (else (cons (car lat)
                        (multisubst new old
                                    (cdr lat)))))))))

;; Add 1 to given number
(define add1
  (lambda (n)
    (+ n 1)))

;; Subtract 1 from given number
(define sub1
  (lambda (n)
    (- n 1)))

;; Same as scheme's +
(define plus
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (add1 (plus n (sub1 m)))))))

;; Same as scheme's -
(define minus
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (sub1 (minus n (sub1 m)))))))

;; Same as scheme's *
(define multiply
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else (plus n (multiply n (sub1 m)))))))

;; Same as scheme's >
(define gt?
  (lambda (n m)
    (cond
     ((zero? n) #f)
     ((zero? m) #t)
     (else (gt? (sub1 n) (sub1 m))))))

;; Same as scheme's <
(define lt?
  (lambda (n m)
    (cond
     ((zero? m) #f)
     ((zero? n) #t)
     (else (lt? (sub1 n) (sub1 m))))))


;; Same as scheme's eq?
(define =?
  (lambda (n m)
    (cond
     ((gt? n m) #f)
     ((lt? n m) #f)
     (else #t))))


;; Same ao sheme's expt
(define power
  (lambda (n m)
    (cond
     ((zero? m) 1)
     (else (multiply n (power n (sub1 m)))))))

;; Same as scheme's /
(define divide
  (lambda (n m)
    (cond
     ((lt? n m) 0)
     (else (add1 (divide (minus n m) m))))))

;; Adds two tuples
(define tup+
  (lambda (tup1 tup2)
    (cond
     ((null? tup1) tup2)
     ((null? tup2) tup1)
     (else
      (cons (plus (car tup1) (car tup2))
            (tup+ (cdr tup1) (cdr tup2)))))))

;; Returno length of list of atoms
(define length
  (lambda (lat)
    (cond
     ((null? lat) 0)
     (else (add1 (length (cdr lat)))))))


;; Return nth atom from a list of atoms. First item starts at 1
(define pick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (car lat))
     (else (pick (sub1 n) (cdr lat))))))


;; Returns a new lat with the atom at index n removed.
(define rempick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (cdr lat))
     (else (cons (car lat)
                 (rempick (sub1 n)
                          (cdr lat)))))))

;; Remove all numbers from list of atoms
(define no-nums
  (lambda (lat)
    (cond
     ((null? lat) (quote ()))
     (else (cond
            ((number? (car lat))
             (no-nums (cdr lat)))
            (else (cons (car lat)
                        (no-nums
                         (cdr lat)))))))))
