(load "./../src/02-do-it-do-it-again.scm")
(load "./../lib/test-manager/load.scm")

(in-test-group
 02-do-it-do-it-again-and-again-and-again
 (define-test (atom-test)
   (assert-true (lat? '(Jack Sprat could eat no chicken fat))
                "many symbols is as list is a lat")
   (assert-false (lat? '((Jack) could eat no chicken fat))
                 "sexp of symbols is not lat")
   (assert-true (lat? '())
                "empty list is a lat"))
 (define-test (member-test)
   (assert-false (member? 'poached '(fried eggs and scrambled eggs))
                 "Symbol is not in list")
   (assert-true (member? 'meat '(mashed potatoes and meat gravy))
                "Symbol is in list")
   (assert-false (member? 'liver '())
                 "Symbol is not in empty list")))

(run-registered-tests)
