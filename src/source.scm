
(load "00-common.scm")

(atom? 'turkey)
;; #t

(atom? 1492)
;; #t

(atom? 'u)
;; #t

(atom? '*abc$)
;; #t

'(atom)
'(atom turkey or)
'((atom turkey) or )
'(x y z)
'((x y) z)
'(how are you doing so far)
'(((how ) are) ((you) (doing so)) far)
'() 

(atom? ())
;;#f

'(() () ())

(car '(a b c))
;;a

(car '((a b c) x y z))
;; (a b c)

(car 'hotdog)
;; No Answer

(car '())
;; No Answer

(car '(((hotdogs)) (and) (pickle) relish))

;; ((hotdogs))

(car (car '(((hotdogs)) (and))))
;; (hotdogs)

;;; The Law of Car
;;; The primitive car in defined only for non-emyty lists.

(cdr '(a b c))
;; (b c)

(cdr '((a b c) x y z))
;; (x y z)

(cdr '(hamburger))
;; ()

(cdr '((x) t r ))
;; (t r)

(cdr '(hotdogs))
;; ()

(cdr '())
;; bad news

;;; The Law of Cdr
;;; The primitive cdr is defined only for non-empty lists. The cdr of
;;; anf non-empty list is always another list.

(car (cdr '((b) (x y ) ((c)))))
;; (x y)

(cdr (cdr '((b) (x y) ((c)))))
;; (((c)))

(cdr (car '(a (b (c)) d)))
;; nope

(cons 'peanut '(butter and jelly))
;; (peanut butter and jelly)

(cons '(banana and) '(peanut butter and jelly))
;; ((banana and) peanut butter and jelly)

(cons '((help) this) '(is very ((hard) to learn)))
;; (((help) this) is very ((hard) to learn))

(cons '(a b (c)) '())
;; ((a b (c)))

(cons 'a ())
;; (a)

(cons '((a b c)) 'b)
;; (((a b c)) . b)
;; did not expect this to work. What is the . ?
;;
;;
(car  (cons '((a b c)) 'b)) ;; (a b c)
(cdr (cons '((a b c)) 'b)) ;; b

;; so the cdr was not a list. hmmmm


(cons 'a 'b)
;;  (a . b)
;; This must be a dotted pair

(pair? (cons 'a 'b))
;; #t
;; getting ahead of the book hear


;;; The Law of cons
;;; The primitive cons takes two arguments. The second argument must
;;; be a list. The result is a list.

(cons 'a (car '((b) c d)))
;Value:  (a b)

(cons 'a (cdr '((b) c d )))
;; (a c d)

(null? '())
;Value: #t

(null? (quote ()))
;Value: #t

(null? '(a b c))
;Value: #f

(null? 'spaghetti)
;Value: #f


;;; The Law of Null?
;;; The primitive null? is defined only for lists.


(atom? 'Harry)
;;Value: #t

(atom? '(Harry had a heap of apples))
;; Value: #f

(atom? (car '(Harry had a heap of apples)))
;; Value: #t


(atom? (cdr '(Harry had a heap of apples)))
;; Value: #f

(atom? (cdr '(Harry)))
;; Value: #f

(atom? (car (cdr '(swing low sweet cherry oat))))
;; Value: #t

(atom? (car (cdr '(swing (low sweet) cherry oat))))
;; Value: #f

(eq? 'Harry 'Harry)
;; Value: #t

(eq? 'margarine 'butter)
;; Value: #f

(eq? '()  '(strawberry))
;; Value: #f

(eq? 6 7)
;; Value: #f

;;; The Law of Eq?
;;; The primitive eq? must be a non-numeric atom.

(eq? '() '())
;;; #t
;;; interesting

(eq? '(hh) '(hh))
;;; #f


(eq? 2 2)
;;; #t
;;; again... interesting....


(eq? (car '(Mary had a little lamb chop)) 'Mary)
;; Value: #t

(eq? (cdr '(soured milk) 'milk))
;; no dice

(eq? (car '(beans beans we need jelly beans))
     (car (cdr '(beans beans we need jelly beans))))
;; Value: #t
