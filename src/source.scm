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

;; Next to funtion searches a list for a Symbol in the
;; first position of the list. If the value found is a number
;; the position defined by the number will be searced for the symbol.
;; This process continues until a symbol in encountered.
;; This funtion is interesting because it is partial - is other
;; words, it is not guaranted to complete for all of it's domain.
(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define keep-looking
  (lambda (a sorn lat)
    (cond ((number? sorn)
           (keep-looking a (pick sorn lat) lat))
          (else (eq? sorn a)))))

;; this would never return
;; (looking 'caviar '(7 1 2 caviar 5 6 3))

;; a very partial funtion!!!
(define eternity
  (lambda (x)
    (eternity x)))

;; take a pair in which the first part is also a pair.
;; builds a new pair by shifting the second part of the
;; first pair into the second part of the main pair.
(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair))
                  (second pair)))))

(define align
  (lambda (pora)
    (cond ((atom? pora) pora)
          ((a-pair? (first pora))
           (align (shift pora)))
          (else (build (first pora)
                       (align (second pora)))))))

;; count number of pair elements
(define length*
  (lambda (pora)
    (cond ((atom? pora) 1)
          (else (+ (length* (first pora))
                   (length* (second pora)))))))

(define weight*
  (lambda (pora)
    (cond ((atom? pora) 1)
          (else (+ (* (weight* (first pora)) 2)
                   (weight* (second pora)))))))

;; partal function
(define shuffle
  (lambda (pora)
    (cond ((atom? pora) pora)
          ((a-pair? (first pora))
           (shuffle (revpair pora)))
          (else (build (first pora)
                       (shuffle (second pora)))))))

;; this will not return
;; (shuffle '((a b) (c d)))

;; partial. does not return for zero.
;; Lothar Callatz
;; Collatz conjecture -- very cool
(define C
  (lambda (n)
    (cond ((one? n) 1)
          (else (cond ((even? n) (C (/ n 2)))
                      (else (C (add1 (* 3 n)))))))))

;; this is total
;; Wilhelm Ackermann
;; this in knows as the Ackermann funtion
;; from wikipedia
;; In computability theory, the Ackermann function, named after
;; Wilhelm Ackermann, is;;  one of the simplest and earliest-discovered
;; examples of a total computable function that is not primitive recursive.
(define A
  (lambda (n m)
    (cond ((zero? n) (add1 m))
          ((zero? m) (A (sub1 n) 1))
          (else (A (sub1 n)
                   (A n (sub1 m)))))))

;; What is define?????

;; we want to find a way to define length without using define


;; this would work for a list of one item
(define x
  (lambda (l)
    (cond ((null? l) 0)
          (else (add1 (eternity (cdr l)))))))


;; this would work for a list with two items but sucks. 
(define x
  (lambda (l)
    (cond ((null? l) 0)
          (else (add1 ((lambda (l)
                         (cond ((null? l) 0)
                               (else (add1
                                      (eternity (cdr l))))))
                       (cdr l)))))))


;; lets abstract out the core function. Pretend eternity is
;; our subsequent call. now this will work for a list of size one....
;; for size 2 we just need to pass in the next call.. but how...
;; eternity needs to macth the inner lambda.

(define x
  ((lambda (length)
     (lambda (l)
       (cond ((null? l) 0)
             (else (add1 (length (cdr l)))))))
   eternity))


;; Another try at making this work on a list with two atoms. This
;; still sucks. We are repeating are self still. 

(define x
  ((lambda (f)
     (lambda (l)
       (cond ((null? l) 0)
             (else (add1 (f (cdr l)))))))
   ((lambda (g)
      (lambda (l)
        (cond ((null? l) 0)
              (else (add1 (g (cdr l)))))))
    eternity)))


;;; this looks interesting. Will only work for list with one item but i like
;; where this is going.

(define x
  ((lambda (mk-length)
     (mk-length eternity)) ;; call builder passing in next call
   (lambda (length) ;; i am mk-length; 
     (lambda (l)
       (cond ((null? l) 0)
             (else (add1 (length (cdr l)))))))))

;; This is an imrovement! We can now get by writing code for lenght once!!!
;; This will work for a list of size 2 or less. We need to make this work
;; for list of size n.

(define x
  ((lambda (mk-length) ;; invoker
     (mk-length
      (mk-length eternity)))
   (lambda (length) ;; builder 
     (lambda (l)   ;; length fn
       (cond ((null? l) 0)
             (else (add1 (length (cdr l)))))))))


;; something like this looks really close but i don't think it will work.
;; (mk-length (cdr l)) is a problem. mk-length needs a lenght funtion
;; ond not a list of atoms.

(define x
  ((lambda (mk-length)
     (mk-length mk-length))
   (lambda (mk-length)
     (lambda (l)
       (cond ((null? l) 0)
             (else (add1 (mk-length (cdr l)))))))))
     


;; Check this out!!!!! This looks pretty good
(define candidate-x1
  ((lambda (mk-length) ;; what would you call this?
     (mk-length mk-length)) ; call create to get a new lenght
   (lambda (mk-length)  ; create length fn
     (lambda (l) ; length fn
       (cond ((null? l) 0)
             (else (add1
                    ((mk-length mk-length) ; call create to get a net length
                     (cdr l)))))))))


;; I got all excited because the above worked but little schemer points
;; out that we have lost our notion of a length function. We now recurse
;; on a mk-length call. I can see how that could be confusing.


;; This is a really fucked up function. def a can't define.
;(define x
;  ((lambda (mk-length)
;     (mk-length mk-length))
;   (lambda (mk-length)
;     ((lambda (length)  
;        (lambda (l) 
;          (cond ((null? l) 0)
;                (else (add1 (length (cdr l)))))))
;      (mk-length mk-length)))))
     
