(load "00-common.scm")

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





