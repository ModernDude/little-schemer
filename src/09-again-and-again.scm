;; Is x an atom
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;; determine if atom is number 1
(define one?
  (lambda (n)
    (=? n 1)))

;; Add 1 to given number
(define add1
  (lambda (n)
    (+ n 1)))

;; Return nth atom from a list of atoms. First item starts at 1
(define pick
  (lambda (n lat)
    (cond
     ((one? n) (car lat))
     (else (pick (sub1 n) (cdr lat))))))

;; Subtract 1 from given number
(define sub1
  (lambda (n)
    (- n 1)))

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

;; This is a really fucked up function. can't define.
;;(define x
;;  ((lambda (mk-length)
;;     (mk-length mk-length))
;;   (lambda (mk-length)
;;     ((lambda (length)
;;        (lambda (l)
;;         (cond ((null? l) 0)
;;                (else (add1 (length (cdr l)))))))
;;      (mk-length mk-length)))))

;; here we wrap the application of mk-length in a lambda
;; why? not sure yet....
(define candidate-x2
  ((lambda (mk-length) ;; what would you call this?
     (mk-length mk-length)) ; call create to get a new lenght
   (lambda (mk-length)  ; create length fn
     (lambda (l) ; length fn
       (cond ((null? l) 0)
             (else (add1
                    ((lambda (x) ; wrap!!!!!
                       ((mk-length mk-length) x))
                     (cdr l)))))))))

;; wow!!! this is a slick function. i see why we made the change above.
;; the wrapped lambda has become our inner lenght funtion to recurse on.
;; it makes the main lenght funtion much more cleaner. we don't have to mix
;; in detetails of "mk-length"
(define candidate-x3
  ((lambda (mk-length) ;; what would you call this?
     (mk-length mk-length)) ; call create to get a new length
   (lambda (mk-length)  ; create length fn
     ((lambda (length)
        (lambda (l) ; length fn
          (cond ((null? l) 0)
                (else (add1 (length (cdr l)))))))
      (lambda (x)
        ((mk-length mk-length) x))))))

;; ok, i thought the above was good enough but little schemer is bent on
;; teaching me something here. On the surface i like how length in setup
;; at the bottom. It is very readable. The top is very hard to follow at
;; first............
;;; ......
;;;....... ok after walking through.. i have it. hell of a funtion
;;
;; advantage is that the function that creates length is separated from
;; the function that looks like lenght
(define candidate-x4
  ((lambda (le) ;;;; applicative order y-combinator
     ((lambda (mk-length)
        (mk-length mk-length))
      (lambda (mk-length)
        (le (lambda (x) ;;
              ((mk-length mk-length) x))))))
   (lambda (length)
     (lambda (l) ; length fn
       (cond ((null? l) 0)
             (else (add1 (length (cdr l)))))))))
