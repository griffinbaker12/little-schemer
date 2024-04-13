#lang scheme

; really cool that anything we define here gets loaded into the repl below

(define atom?
  (lambda (x)
    (and (not (null? x)) (not (pair? x)))))

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? l) #f)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(define test-lst
  (lambda (l)
    (cond
      ((null? l) 0)
      ((lat? l) "list of atoms")
      (else (car l)))))

; Create an expression as a list
(define code (list `+ 2 3))

(define set-second
  (lambda (lst new-val)
    (if (null? (cdr lst))
        (cons new-val car(lst))
        (cons (car lst)
              (cons new-val
                    (cddr lst))))))

(set! code (set-second code 10))
(display code)  ; Output: (+ 10 3)

(define member?
  (lambda (lst mem)
    (cond
      ((null? lst) #f)
      (else (or (eq? (car lst) mem)
                (member? (cdr lst) mem))))))

; (rember a lat) -> (rember `and `(bacon lettuce and tomato))
; (cons `bacon (rember `(lettuce and tomato)))

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) `())
      ; returns right away (but need to keep recursing in the multi-remove case
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat) (rember a (cdr lat)))))))

(define firsts
  (lambda (l)
    (cond
      ((null? l) `())
      (else (cons (caar l) (firsts (cdr l)))))))

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) `())
      ((eq? (car lat) old) (cons old (cons new (cdr lat))))
      (else (cons (car lat) (insertR new old (cdr lat)))))))
      

(define lst (list `a `b `c `d `f `g))
(insertR `e `d lst)

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) `())
      ((eq? (car lat) old) (cons new lat))
      (else (cons (car lat) (insertL new old (cdr lat)))))))

(insertL `e `f lst)

(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) `())
      ((eq? (car lat) old) (cons new (cdr lat)))
      (else (cons (car lat) (subst new old (cdr lat)))))))

(subst `e `f lst)

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) `())
      ((or (eq? (car lat) o1) (eq? (car lat) o2)) (cons new (cdr lat)))
      (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

(subst2 `vanilla `chocolate `banana (list `banana `ice `cream `with `chocolate `topping))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) `())
      ; both cases here move us closer to the base case of the emtpy list
      
      ; but this saves the value
      ((eq? (car lat) a) (multirember a (cdr lat)))
      ; and this doesn't
      (else (cons (car lat) (multirember a (cdr lat)))))))

(define dup-lst (list 1 2 3 1 5 1 1))
(multirember 1 dup-lst)

(define multiinsertR
  (lambda (new old lat)
    (cond
    ((null? lat) `())
    ((eq? (car lat) old) (cons old (cons new (multiinsertR new old (cdr lat)))))
    (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

(define lst-multiple (list `a `b `c `d `f `g `d `z `y `d))
(multiinsertR `e `d lst-multiple)

(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) `())
      ((eq? (car lat) old) (cons new (cons old (multiinsertL new old (cdr lat)))))
      (else (cons (car lat) (multiinsertL new old (cdr lat))))))) 

(multiinsertL `e `d lst-multiple)

(define multisubst













  






