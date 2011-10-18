3;; Is x an atom
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
     ((one? n) (car lat))
     (else (pick (sub1 n) (cdr lat))))))

;; Returns a new lat with the atom at index n removed.
(define rempick
  (lambda (n lat)
    (cond
     ((one? n) (cdr lat))
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

;; Extract numbers from list
(define all-nums
  (lambda (lat)
    (cond
     ((null? lat) (quote ()))
     (else
      (cond
       ((number? (car lat))
        (cons (car lat)
              (all-nums (cdr lat))))
       (else (all-nums (cdr lat))))))))

;; Equal if the same atom
(define eqan?
  (lambda (a1 a2)
    (cond
     ((and (number? a1) (number? a2))
      (=? a1 a2))
     ((or (number? a1) (number? a2))
      #f)
     (else (eq? a1 a2)))))

;; Count number times atom appears in list
(define occur
  (lambda (a lat)
    (cond
     ((null? lat) 0)
     (else
      (cond
       ((eqan? (car lat) a)
        (add1 (occur a (cdr lat))))
       (else (occur a (cdr lat))))))))

;; determine if atom is number 1
(define one?
  (lambda (n)
    (=? n 1)))

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
