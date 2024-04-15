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
  (lambda (new old lat)
    (cond
      ((null? lat) `())
      ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
      (else (cons (car lat) (multisubst new old (cdr lat)))))))
(multisubst `e `d lst-multiple)

(define add1
  (lambda (n)
    (+ n 1)))
(add1 10)

(define sub1
  (lambda (n)
    (- n 1)))
(sub1 (add1 10))
(zero? 0)

(define +o
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (+o n (sub1 m)))))))
(+o 46 12)
  
(define -o
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (-o n (sub1 m)))))))
(-o 46 12)

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (+ (car tup) (addtup (cdr tup)))))))
(define tup `(1 2 3 4 5))
(addtup tup)

(define iseven
  (lambda (num)
    (cond
      ((zero? num) #t)
      (else (not (iseven (sub1 num)))))))
(iseven 10)
(iseven 11)
(iseven 98)
(iseven 101)

(define *o
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (+o n (*o n (sub1 m)))))))
(*o 5 3)
(*o 5 8)
(*o 13 3)
(*o 3 13)

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else
       (cons (+o (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))
(tup+ (list 1 2 3) (list 1 2 3))
(tup+ (list 3 7) (list 4 6))
(tup+ (list 4 6) (list 2))

(define >o
  (lambda (n m)
    (cond
      ((zero? n) false)
      ((zero? m) true)
      (else (>o (sub1 n) (sub1 m))))))

(>o 10 3)
(>o 3 10)
(>o 3 3)

(define <o
  (lambda (n m)
    (cond
      ((zero? m) false)
      ((zero? n) true)
      (else (<o (sub1 n) (sub1 m))))))
(<o 10 3)
(<o 3 10)
(<o 3 3)

(define =o
  (lambda (n m)
    (cond
      ((< n m) #f)
      ((> n m) #f)
      (else #t))))
(=o 5 5)
(=o 3 5)

(define exp
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (*o n (exp n (sub1 m)))))))
(exp 10 3)
(exp 5 2)

; so cool...count how many times you are doing this operation
(define div
  (lambda (n m)
    (cond
      ((< n m) 0)
      (else (add1 (div (-o n m) m))))))
(div 100 20)
(div 100 10)

(define length
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length (cdr lat)))))))
(length (list 1 2))
(length (list 1 2 3 5))

(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))
(pick 3 (list 1 2 3 4))

(define rempick
  (lambda (n lat)
    (cond
      ((zero? n) (sub1 n) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))
(rempick 3 (list 1 2 3 4))
(rempick 2 (list 1 2 3 4))
(rempick 0 (list 1 2 3 4))

(define lst1 (list `a 1 2 `b))
(define lst2 (list 5 `pears 6 `plums 8 `dates))

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) `())
      ((number? (car lat)) (no-nums (cdr lat)))
      (else (cons (car lat) (no-nums (cdr lat)))))))
(no-nums lst1)
(no-nums lst2)

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) `())
      ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
      (else (all-nums (cdr lat))))))
(all-nums lst1)
(all-nums lst2)

(define eqan?
  (lambda (a b)
    (cond
      ((and (number? a) (number? b) (= a b)))
      ((or (number? a) (number? b)) #f)
      (else (eq? a b)))))
(eqan? 1 3)
(eqan? 3 3)
(eqan? `a `a)
(eqan? `a `b)
       
(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((= (car lat) a) (add1 (occur a (cdr lat))))
      (else (occur a (cdr lat))))))
(occur 1 (list 1 1 1 1))
(occur 1 (list 1 3 1 5 1))

(define one?
  (lambda (n)
    (= n 1)))
(one? 1)
(one? 2)

(define rempickx
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else (cons (car lat) (rempickx (sub1 n) (cdr lat)))))))
(rempickx 3 `(`lemon `meringue `salty `pie))

(define rember*
  (lambda (a l)
    (cond
      ((null? l) `())
      ((atom? (car l))
       (cond
         ((eq? (car l) a)
          (rember* a (cdr l)))
         (else (cons (car l) (rember* a (cdr l))))))
      (else (cons (rember* a (car l)) (rember* a (cdr l)))))))
(define l `((coffee) cup ((tea) cup) (and (hick)) cup))
(rember* `cup l)

(define insertR*
  (lambda (new old l)
    (cond
    ((null? l) `())
    ((atom? (car l))
     (cond
       ((eq? (car l) old)
        (cons old (cons new (insertR* new old (cdr l)))))
       (else (cons (car l) (insertR* new old (cdr l))))))
    (else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))
(define li `((how much (wood)) could ((a (wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood))
(insertR* `roast `chuck li)

(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (cond
         ((eq? (car l) a)
          (add1 (occur* a (cdr l))))
         (else (occur* a (cdr l)))))
      (else (+o (occur* a (car l)) (occur* a (cdr l)))))))
(define li2 `((banana) (split (((banana ice))) (cream (banana)) (sherbet)) (banana) (bread) (banana brandy)))
(occur* `banana li2)

(define subst*
  (lambda (new old l)
    (cond
      ((null? l) `())
      ((atom? (car l))
       (cond
         ((eq? (car l) old)
          (cons new (subst* new old (cdr l))))
         (else (cons (car l) (subst* new old (cdr l))))))
      (else (cons (subst* new old (car l)) (subst* new old (cdr l)))))))
(subst* `apple `banana li2)              

(define insertL*
  (lambda (new old l)
    (cond
    ((null? l) `())
    ((atom? (car l))
     (cond
       ((eq? (car l) old)
        (cons new (cons old (insertL* new old (cdr l)))))
       (else (cons (car l) (insertL* new old (cdr l))))))
    (else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))
(insertL* `roast `chuck li)
             
(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
       (or (eq? (car l) a) (member* a (cdr l))))
      (else (or (member* a (car l)) (member* a (cdr l)))))))
(member* `chuck li)
(member* `duck li)

; interesting one in that it keeps drilling to the left of the list that you pass
; agreed that the list is non-empty and does not contain a non-empty list
(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))
(leftmost li)

; each arg may be either
; - empty
; - an arg consed onto a list
; - a list consed onto a list
; and you also need to check the state of the other args when one arg is held constant
(define eqlistinit?
  (lambda (l1 l2)
    (cond
      ; case where l1 is empty, iterate over l2 cases
      ;1
      ((and (null? l1) (null? l2) #t))
      ;2
      ((and (null? l1) (atom? (car l2)) #f))
      ; because l2 isn't null, nor is there an atom there, so must be a list in the car position
      ;3
      ((null? l1) #f)

      ; case where l1 has an atom in the first position, iterate over l2 cases
      ;4
      ((and (atom? (car l1)) (null? l2)) #f)
      ;5
      ((and (atom? (car l1))
            (atom? (car l2)))
       (and (eqan? (car l1) (car l2))
            (eqlistinit? (cdr l1) (cdr l2))))
      ;6
      ((atom? (car l1) #f))

      ; case where l1 has a list in the first position
      ;7
      ((null? l2) #f)
      ;8
      ((atom? (car l2)) #f)
      ;9
      (else
       (and (eqlistinit? (car l1) (car l2))
            (eqlistinit? (cdr l1) (cdr l2)))))))

(define eqlist?
  (lambda (l1 l2)
    (cond
      ; either both are null, and they are the same, or one is and the other is not, so they are not the same
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)

      ; the use of the and with ors is really clever here, either they are both empty, or if one of them is, then we know we are at a false case
      ;if they are both atoms, they better be equal
      ((and (atom? (car l1))
            (atom? (car l2)))
       (and (eqan? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2))))
      ; if one of them is atoms, and the other isn't, then false!
      ((or (atom? (car l1)) #f)
      ((atom? (car l2)) #f))
      
      ; exhausted all other options, so the lists in the first position must be equal!
      (else
       (and (eqlist? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2)))))))
(eqlist? (list 1 2 3) (list 1 2 3))
(eqlist? (list 1 2 3) (list 1 2 4))

; S-expression is either an atom or a (potentially empty) list of S-expressions
; So we just tack on the check for atoms here before passing to eqlist?
(define equal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2))
       (eqan? s1 s2))
      ((or (atom? s1) (atom? s2)) #f)
      (else (eqlist? s1 s2)))))
(equal? 2 3)
(equal? 2 (list 1 2))
(equal? `(1 (2) (3 4 (5))) `(1 (2) (3 4 (5))))

; (define eqlist?
  ;(lambda (l1 l2)
   ; (cond
    ;  ((and (null? l1) (null? l2)) #t)
     ; ((or (null? l1) (null? l2)) #f)
      ;(else (and (equal? (car l1) (car l2)) (equal? (cdr l1) (cdr l2)))))))

; now removes the first matching s expression from a list of s-expressions as opposed to the first matching
; atom in a list of atoms
(define rembers
  (lambda (a l)
    (cond
      ((null? l) `())
      ; returns right away (but need to keep recursing in the multi-remove case)
      ; equal lets us check against list of s-expressions which we already tested
      ((equal? (car l) a) (cdr l))
      (else (cons (car l) (rembers a (cdr l)))))))

(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else
       (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))))))
(numbered? 10)
(numbered? `(10 + 5))
(numbered? `(2 * sausage))
(numbered? `(2 * (5 + sausage)))

; defining new ways to 'represent' arithmetic expressions! sick...
(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (car (cdr nexp)) `+)
       (+ (value (car nexp)) (value (car (cddr nexp)))))
      ((eq? (car (cdr nexp)) `*)
       (* (value (car nexp)) (value (car (cddr nexp)))))
      (else
       (expt (value (car nexp)) (value (car (cddr nexp))))))))
(value `(3 + 5))
(value `(3 + (8 * 10)))
(value `(3 + (2 * (4 exp 1))))















