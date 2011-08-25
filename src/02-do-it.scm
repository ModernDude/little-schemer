
(load "00-common.scm")

;;; lat? defined in common

(lat? '(Jack Sprat could eat no chicken fat))
;; #t

(lat? '((jack) Sprat could eat no chicken fat))
;; #f

(lat? '(Jack (Sprat could) eat no chicken fat))
;; #f

(lat? '())
;; #t

(member? 'tea '(cofee tea or milk))
;; #t

(member? 'poached '(fried eggs and scrambled eggs))
;; #f

(member? 'meat '(mashed potatoes and meat gravy))
;; #t

;;; The First Commandment
;;; Always ask null? as the first question in expressing anf function.

(member? 'liver '(bagels and lox))
;; #f

(member? 'liver '())
;; #f

