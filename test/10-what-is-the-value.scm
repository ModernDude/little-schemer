(load "./../src/10-what-is-the-value.scm")
(load "./../lib/test-manager/load.scm")

(in-test-group
 entry-tests
 (define-test (lookup-existing-name-returns-value)
   (assert-equal
    'mon
    (lookup-in-entry 'm (new-entry '(m t w) '(mon tues wed)) (lambda (x) x))))
 (define-test (lookup-not-found-name-returns-name)
   (assert-equal
    'not-found
    (lookup-in-entry 'not-found (new-entry '(m t w) '(mon tues wed)) (lambda (x) x)))))

(in-test-group
table-tests
 (define-test (lookup-existing-name-in-first-entry-returns-value)
 (assert-equal 'spaghetti
                (lookup-in-table
                 'entree
                 '(((entree dessert)
                    (spaghetti spumoni))
                   ((appetizer entree2 beverage)
                    (food tastes good)))
                 (lambda (name) 'not-found))))
 (define-test (lookup-existing-name-in-second-entry-returns-value)
   (assert-equal 'tastes
                 (lookup-in-table
                 'entree2
                 '(((entree dessert)
                    (spaghetti spumoni))
                   ((appetizer entree2 beverage)
                    (food tastes good)))
                 (lambda (name) 'not-found))))
 (define-test (lookup-missing-name-retuns-not-found)
   (assert-equal 'not-found
                 (lookup-in-table
                 'not-found
                 '(((entree dessert)
                    (spaghetti spumoni)))
                 (lambda (name) 'not-found)))))


(in-test-group
action-*const-tests
(define-test (number-returns-itself)
  (assert-equal 3 (*const 3 '())))
(define-test (true-returns-true)
  (assert-equal #t (*const #t '())))
(define-test (false-returns-fallse)
  (assert-equal #f (*const #f '())))
(define-test (symbol-returns-primitive)
  (assert-equal '(primitive blah)
                (*const 'blah '()))))
 
(in-test-group
action-*quote-tests
(define-test (second-val-as-atom-is-returned)
  (assert-equal
   'abc
   (*quote '(quote abc) '())))
(define-test (second-val-as-list-is-returned)
  (assert-equal
   '(a b c)
   (*quote '(quote (a b c)) '()))))

(in-test-group
action-*identifier-tests
(define-test (first-value-found-in-table-is-returned)
  (assert-equal
   'aa
   (*identifier 'a '(((a b)(aa bb)) ((a b) (aaa bbb)))))))

(in-test-group
action-*lambda-tests
(define-test (lambda-returns-primitive-with-formals-and-env)
  (assert-equal '(non-primitive ((((y z) ((8) 9))) (x) (cons x y) ))
                (*lambda '(lambda (x) (cons x y)) '(((y z) ((8) 9)))))))


(in-test-group
action-*cond-tests
(define-test (only-else-retuns-env-val)
  (assert-equal
   20
   (*cond '(cond (else x)) '(((x)(20))))))
(define-test (matching-condition-returns-env-val)
  (assert-equal
   'found
   (*cond '(cond ((eq? x 20) (quote found)) (else 'not-found))
          '(((x) (20)))))))
 
           
(in-test-group
action-*application-tests
(define-test (given-literal-return-identity)
  (assert-equal
   20
   (*application '((lambda (x) x) 20) '(((x)(1))))))
(define-test (given-identifier-return-identity)
  (assert-equal
   20
   (*application '((lambda (x) x) x) '(((x)(20))))))
(define-test (2-diff-identifiers-return-false)
  (assert-equal
   #f
   (*application '((lambda (x) (eq? x y)) x)
                 '(((x)(20))((y)(10)))))))


(in-test-group
 atom-to-action
 (define-test (number?-returns-*const)
   (assert-equal *const (atom-to-action 'number?)))
 (define-test (true-returns-*const)
   (assert-equal *const (atom-to-action '#t)))
 (define-test (false-returns-*const)
   (assert-equal *const (atom-to-action '#f)))
 (define-test (cons-returns-*const)
   (assert-equal *const (atom-to-action 'cons)))
 (define-test (car-returns-*const)
   (assert-equal *const (atom-to-action 'car)))
 (define-test (cdr-returns-*const)
   (assert-equal *const (atom-to-action 'cdr)))
 (define-test (null?-returns-*const)
   (assert-equal *const (atom-to-action 'null?)))
 (define-test (eq?-returns-*const)
   (assert-equal *const (atom-to-action 'eq?)))
 (define-test (atom?-returns-*const)
   (assert-equal *const (atom-to-action 'atom?)))
 (define-test (zero?-returns-*const)
   (assert-equal *const (atom-to-action 'zero?)))
 (define-test (add1-returns-*const)
   (assert-equal *const (atom-to-action 'add1)))
 (define-test (sub1-returns-*const)
   (assert-equal *const (atom-to-action 'sub1)))
 (define-test (number?-returns-*const)
   (assert-equal *const (atom-to-action 'number?)))
 (define-test (non-primitive-retruns-*identifier)
   (assert-equal *identifier (atom-to-action 'aaaa))))

(in-test-group
 list-to-action
 (define-test (when-input-quote-return-*quote)
   (assert-equal
    *quote
    (list-to-action '(quote (cons a ())))))
 (define-test (when-input-lambda-return-*lambda)
   (assert-equal
    *lambda
    (list-to-action '(lambda (x) x))))
 (define-test (when-input-cond-return-*cond)
   (assert-equal
    *cond
    (list-to-action '(cond (else x)))))
 (define-test (when-input-list-not-qlc-return-*app)
   (assert-equal
    *application
    (list-to-action '((lambda (x) x) 3)))))

(in-test-group
 value
 (define-test (of-identity-fn)
   (assert-equal
    20
    (value '((lambda (x) x) 20))))
 (define-test (of-identity-fn-with-identity-as-input)
   (assert-equal
    20
    (value '((lambda (x) x) ((lambda (x) x) 20)))))
 (define-test (of-identity-fn-with-identity-fn-as-input)
   (assert-equal
    20
    (value '((lambda (f) (f 20)) (lambda (x) x))))))  
   


(run-registered-tests)
