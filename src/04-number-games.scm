;; Is x an atom
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

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
