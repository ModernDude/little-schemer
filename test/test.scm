(load "01-toys.scm")
(load "./../lib/test-manager/load.scm")


(in-test-group
 toys
 (define-test (atom-tests)
   (assert-true (atom? 'turkey) "Symbol is an atom")
   (assert-true (atom? 1492) "Number is an atom")
   (assert-true (atom? 'u) "Sumbol length 1 is an atom")
   (assert-true (atom? '*abc$) "Symbol with special chars is atom")
   (assert-false (atom? '()) "Empty list is not an atom"))
 (define-test (car-tests)
   (assert-equal 'a
                 (car '(a b c))
                 "Car removes first atom in non empty list")
   (assert-equal '(a b c)
                 (car '((a b c) x y z))
                 "Removes a list")
   (assert-equal '((hotdogs))
                 (car '(((hotdogs)) (and) (pickle) relish))
                 "Remove list of list"))
 (define-test (cdr-tests)
   (assert-equal '(b c) (cdr '(a b c))
                 "from non empty list")
   (assert-equal '(x y z) (cdr '((a b c) x y z))
                 "from nested sexp")
   (assert-equal '() (cdr '(hamburger))
                 "empty when list length 1"))
 (define-test (cons-tests)
   (assert-equal '(peanut butter and jelly)
                 (cons 'peanut '(butter and jelly))
                 "Cons symbol onto list")
   (assert-equal '((banana and) peanut butter and jelly)
                 (cons '(banana and) '(peanut butter and jelly))
                 "Cons list onto list")
   (assert-equal '(((help) this) is very ((hard) to learn))
                 (cons '((help) this) '(is very ((hard) to learn)))
                 "cons sexp onto sexp")
   (assert-equal '((a b (c)))
                 (cons '(a b (c)) '())
                 "cons sexp onto empty list")
   (assert-equal '(a)
                 (cons 'a '())
                 "cons symbol onto empty list"))
 (define-test (null-tests)
   (assert-true (null? '()) "empty list is null")
   (assert-false (null? '(a b c)) "non empty list is not null")
   (assert-false (null? 'spaghetti) "symbol is not null"))
 )



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

;;;
;;;
;;;
;;;
;;;
;;;
;;;
;;;
cons

(rember 'mint '(lamb chops and mint jelly))
;; (lamb chops and jelly)

(rember 'mint '(lamb chops and mint flavored mint jelly))
;; (lamb chops and flavored mint jelly)

(rember 'toast '(bacon lettuce and tomato))
;; (bacon lettuce and tomato)

(rember 'cup '(coffee cup tea cup and hick cup))
;; (coffee tea cup and hick cup)

(rember 'bacon '(bacon lettuce and tomato))
;; (lettuce and tomato)

;;; The Second Commandment
;;; Use cons to build lists.

(rember 'sauce '(soy sauce and tomato sauce))
;; (soy and tamato sauce)

(firsts '((apple peach pumpkin)
          (plum pear cherry)
          (grape raisin pea)
          (bean carrot eggplant)))
;;; (apple plum grape bean)

(firsts '((a b) (c d) (e f)))
;;; (a c e)

(firsts '())
;;; ()

(firsts '(((five plums) four)
        (eleven green oranges)
        ((no) more)))
;;; ((five plums) eleven (no))


;;; Third Commandment
;;; When building a list, describe the first typical element, and then
;;; cons it onto the natural recursion.

(insertR 'topping 'fudge '(ice cream with fudge for dessert))
;;; (ice cream with fudge topping for dessert)

(insertR 'jalapeno 'and '(tacos tamales and salsa))
;;; (tacos tamales and jalapeno salsa)

(insertR 'e 'd '(a b c d f g d h))
;;; (a b c d e f g d h)

(insertL 'topping 'fudge '(ice cream with fudge for dessert))
;;; (ice cream with topping fudge for dessert)

(insertL 'jalapeno 'and '(tacos tamales and salsa))
;;; (tacos tamales jalapeno and salsa)

(insertL 'e 'd '(a b c d f g d h))
;;; (a b c e d f g d h)


(subst 'topping 'fudge '(ice cream with topping for dessert))
;;; (ice cream with topping for dessert)

(subst2 'vanilla 'chocolate 'banana '(banana ice cream with chocolate topping))
;;; (vanilla ice cream with chocolate topping)

(subst2 'vanilla 'chocolate 'topping '(banana ice cream with chocolate topping))
;;; (bananna ice cream with vanilla topping)

(multirember 'cup '(coffee cup tea cup and hick cup))
;;; (coffee tea and hick)







(run-registered-tests)


